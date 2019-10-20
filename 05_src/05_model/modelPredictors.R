

# predictNN <- function(dataPath, fileName, indexName){
#   assertString(dataPath)
#   assertString(fileName)
#   assertString(indexName)
# 
#   h2o.init(nthreads=-1, max_mem_size="16G")
#   h2o.removeAll()
#   data <- read.fst(paste0(dataPath, fileName), as.data.table = TRUE)
# 
#   indexes <- read.fst(paste0(dataPath, indexName), as.data.table = TRUE)[[1]] 
#   
#   trainData <- data[indexes]
#   testData <- data[-indexes]
#   testLabel <- as.character(testData$labelRaw)
#   
#   trainData.h2o <- as.h2o(trainData)
#   testData.h2o <- as.h2o(testData)
#   
#   print("fitting model")
#   model <- h2o.deeplearning(y = "labelRaw", training_frame = trainData.h2o, 
#                                 model_id = "1")
#   
#   # predict testData and evaluate metrics
#   predict.h2o <- h2o.predict(object = model, newdata = testData.h2o)
#   predict.local <- as.data.table(predict.h2o)
#   
#   predictions <- as.character(predict.local[, predict])
#   predict.local[, predict := NULL]
#   
#   truth <- oneHotEncode(testData$labelRaw)
#   resultDiffProb <- (truth - predict.local)^2 
#   
#   meanSquareError <- sum(resultDiffProb)/prod(dim(resultDiffProb))
#   accuracy <- mean(testLabel == predictions)
#   
#   return(list(meanSquareError = meanSquareError,
#               accuracy = accuracy,
#               model = model))
# }
# 

predictXG <- function(dataPath, fileName, indexName, labelName,
                      upSampling = FALSE, sparse = FALSE, nrounds = 20){
  assertString(dataPath)
  assertString(fileName)
  assertString(indexName)

  indexes <- read.fst(paste0(dataPath, indexName), as.data.table = TRUE)[[1]] 
  
  if(!sparse) {
    data <- read.fst(paste0(dataPath, fileName), as.data.table = TRUE)
    
    trainData <- data[indexes]
    if(upSampling) trainData <- generalizedSampling(data = trainData, 
                                                    method = "up", 
                                                    label = "labelRaw")
    trainLabelRaw <- trainData$labelRaw
    
    testData <- data[-indexes]
    
    testLabelRaw <- testData$labelRaw
    
    trainLabel <- as.numeric(trainLabelRaw) - 1
    testLabel <- as.numeric(testLabelRaw) - 1
    trainData[, labelRaw := NULL]
    testData[, labelRaw := NULL]
    
    print(paste("in train are number of uniques:", length(unique(trainLabel))))
    print(paste("in test are number of uniques:", length(unique(testLabel))))
    
    # create watchlist
    watchIndexes <- sample.int(nrow(trainData), 
                               size = round(nrow(trainData) * 0.8))
    watchTrain <- trainData[watchIndexes]
    watchTest <- trainData[-watchIndexes]
    watchTrainLabel <- trainLabel[watchIndexes]
    watchTestLabel <- trainLabel[-watchIndexes]
    
    watchTrainMat <- xgb.DMatrix(data = as.matrix(watchTrain), 
                                 label = watchTrainLabel)
    watchTestMat <- xgb.DMatrix(as.matrix(watchTest), 
                                label = watchTestLabel)
    testMat = xgb.DMatrix(as.matrix(testData), label = testLabel)
    
    watchlist = list(dtrain = watchTrainMat, dtest = watchTestMat)
    
  }
 
  if(sparse) {
    data <- readRDS(paste0(dataPath, fileName))
    label <- read.fst(paste0(dataPath, labelName), as.data.table = TRUE)
    
    trainData <- data[indexes,]
    trainLabelRaw <- label$labelRaw[indexes]
    numClass <- length(unique(trainLabelRaw))
    
    testData <- data[-indexes,]
    testLabelRaw <- label$labelRaw[-indexes]
    
    trainLabel <- as.numeric(trainLabelRaw) - 1
    testLabel <- as.numeric(testLabelRaw) - 1
    
    print(paste("in train are number of uniques:", numClass))
    print(paste("in test are number of uniques:", length(unique(testLabelRaw))))
    
    # create watchlist
    watchIndexes <- sample.int(nrow(trainData), 
                               size = round(nrow(trainData) * 0.8))
    watchTrain <- trainData[watchIndexes,]
    watchTest <- trainData[-watchIndexes, ]
    watchTrainLabel <- trainLabel[watchIndexes]
    watchTestLabel <- trainLabel[-watchIndexes]
    
    watchTrainMat <- xgb.DMatrix(data = watchTrain, 
                                 label = watchTrainLabel)
    watchTestMat <- xgb.DMatrix(watchTest, 
                                label = watchTestLabel)
    testMat = xgb.DMatrix(testData, label = testLabel)
    
    watchlist = list(dtrain = watchTrainMat, dtest = watchTestMat)
    
    
  }
  
  # add Parameters
  numClass <- numClass
  eval_metric <- "merror"
  objective <- "multi:softprob"
  nrounds <- nrounds
  
  print("training xgboost model")
  model = xgboost::xgb.train(eval_metric = eval_metric,
                             objective = objective,
                             num_class = numClass,
                             data = watchTrainMat, 
                             nrounds = nrounds, 
                             verbose = 1, 
                             watchlist = watchlist)
  
  # evaluating predictions and eval metrics
  predictions <- as.data.table(predict(model, 
                                       newdata =  testMat,
                                       reshape = TRUE))
  names(predictions) <- levels(testLabelRaw)
  predictionsMax <- apply(predictions, 1 , function(x) {
    names(x[which.max(x)])
  })
  
  # Prob vs Accuracy plot Data
  predictionsMaxProb <- apply(predictions, 1 , function(x) {
    x[which.max(x)]
  })
  correctBinary <- testLabelRaw == predictionsMax
  ProbAccDT <- data.table(Prob = predictionsMaxProb,
                          Correct = correctBinary)
  
  # mse, accuracy
  truth <- to_categorical(testLabel)
  resultDiffProb <- (truth - predictions)^2    
  meanSquareError <- sum(resultDiffProb)/prod(dim(resultDiffProb))
  accuracy <- mean(correctBinary)
  
  confusionMatrix <- matrix(table(factor(predictionsMax, 
                                         levels =  levels(testLabelRaw)),
                                  factor(testLabelRaw, 
                                         levels =  levels(testLabelRaw))), 
                            ncol = numClass)
  
  confAcc <- sum(diag(confusionMatrix)) / sum(confusionMatrix)
  
  # checking accuracy of model and from confusion matrix
  print(paste("model accuracy is", accuracy,
              "and confusion matrix accuracy is", confAcc))
  
  # naming rows and cols
  rownames(confusionMatrix) <- levels(testLabelRaw)
  colnames(confusionMatrix) <- levels(testLabelRaw)
  print(paste("sum of confusionMatrix is ", sum(confusionMatrix)))
  
  # accuracy by class
  accByClass <- diag(as.matrix(confusionMatrix)) / colSums(truth)
  names(accByClass) <-  levels(testLabelRaw)
  
  
  return(list(acc = accuracy,
              meanSquareError = meanSquareError,
              confusionMatrix = confusionMatrix,
              ProbAccDT = ProbAccDT,
              accByClass = accByClass,
              predictions = predictions,
              testLabelRaw = testLabelRaw))

}


predictRF <- function(dataPath, fileName, indexName, labelName = NULL,
                      upSampling = FALSE, sparse = FALSE, num.trees = 500) {
  assertString(dataPath)
  assertString(fileName)
  assertString(indexName)


  # loading indexis for train/test split
  indexes <- read.fst(paste0(dataPath, indexName), as.data.table = TRUE)[[1]]  
  
  if(!sparse){
    data <- read.fst(paste0(dataPath, fileName), as.data.table = TRUE)
    trainData <- data[indexes,]
    if(upSampling) trainData <- generalizedSampling(data = trainData, 
                                                    method = "up", 
                                                    label = "labelRaw")
    trainLabelRaw <- trainData$labelRaw
    testData <- data[-indexes, ]
    testLabelRaw <- testData$labelRaw
    
    trainLabel <- as.numeric(trainLabelRaw) - 1
    testLabel <- as.numeric(testLabelRaw) - 1
    
    print("fitting model with data.table")
    model = ranger::ranger(dependent.variable.name = "labelRaw", 
                           data = trainData,
                           importance = "impurity",
                           probability = TRUE, num.trees = num.trees)
    
    predictions = as.data.table(predict(model, 
                                        testData, 
                                        type = "response")$predictions)
    
    # evaluating predictions and eval metrics
    names(predictions) <- levels(testLabelRaw)
    
    predictionsMax <- apply(predictions, 1 , function(x) {
      names(x[which.max(x)])
    })
    
    # Prob vs Accuracy plot Data
    predictionsMaxProb <- apply(predictions, 1 , function(x) {
      x[which.max(x)]
    })
    correctBinary <- testLabelRaw == predictionsMax
    ProbAccDT <- data.table(Prob = predictionsMaxProb,
                            Correct = correctBinary)
    
    
    # mse, accuracy
    # this one is in order. needs to be rearranged for a comparison with
    # predictions
    truth <- to_categorical(testLabel)[, orderOfNames]
    resultDiffProb <- (truth - predictions)^2    
    meanSquareError <- sum(resultDiffProb)/prod(dim(resultDiffProb))
    accuracy <- mean(testLabelRaw == predictionsMax)
    
    confusionMatrix <- matrix(table(factor(predictionsMax, 
                                           levels =  levels(testLabelRaw)),
                                    factor(testLabelRaw, 
                                           levels =  levels(testLabelRaw))), 
                              ncol = length(levels(testLabelRaw)))
    
    confAcc <- sum(diag(confusionMatrix)) / sum(confusionMatrix)
    
    # checking accuracy of model and from confusion matrix
    print(paste("model accuracy is", accuracy,
                "and confusion matrix accuracy is", confAcc))
    
    
    # naming rows and cols
    rownames(confusionMatrix) <- levels(testLabelRaw)
    colnames(confusionMatrix) <- levels(testLabelRaw)
    print(paste("sum of confusionMatrix is ", sum(confusionMatrix)))
    
    # accuracy by class
    accByClass <- diag(as.matrix(confusionMatrix)) / colSums(truth)
    names(accByClass) <-  levels(testLabelRaw)
    
  }
  
  if(sparse){
    data <- readRDS(paste0(dataPath, fileName))
    label <- read.fst(paste0(dataPath, labelName), as.data.table = TRUE)
    trainData <- data[indexes,]
    trainLabelRaw <- label[indexes]$labelRaw
    testData <- data[-indexes, ]
    testLabelRaw <- label[-indexes]$labelRaw

    # because of naming transition use not "-1"
    trainLabel <- Matrix::Matrix(data = as.numeric(trainLabelRaw),
                                 sparse = TRUE, ncol = 1)
    dimnames(trainLabel)[[2]] <- "trainLabel"
    testLabel <- as.numeric(testLabelRaw)
    
    trainSparse <- cbind(trainLabel, trainData)
    testSparse <- cbind(testLabel, testData)
    
    
    print("fitting model with sparse")
    model = ranger::ranger(dependent.variable.name = "trainLabel", 
                           data = trainSparse,
                           importance = "impurity",
                           probability = TRUE, num.trees = num.trees,
                           classification = TRUE, verbose = TRUE)
    # when calc. with a sparse matrix, the column names are ordered along
    # the true classes of the first training examples
    orderOfNames <- as.integer(colnames(model$predictions))
    
    predictions = as.data.table(predict(model, 
                                        testData, 
                                        type = "response")$predictions)
    
    # evaluating predictions and eval metrics
    names(predictions) <- levels(testLabelRaw)[orderOfNames]
    
    rm(data)
    gc()
    
    
    predictionsMax <- apply(predictions, 1 , function(x) {
      names(x[which.max(x)])
    })
    
    # Prob vs Accuracy plot Data
    predictionsMaxProb <- apply(predictions, 1 , function(x) {
      x[which.max(x)]
    })
    correctBinary <- testLabelRaw == predictionsMax
    ProbAccDT <- data.table(Prob = predictionsMaxProb,
                            Correct = correctBinary)
    
    
    # mse, accuracy
    # this one is in order. needs to be rearranged for a comparison with
    # predictions ToDo: check this one
    truth <- to_categorical(testLabel)[, orderOfNames]
    resultDiffProb <- (truth - predictions)^2    
    meanSquareError <- sum(resultDiffProb)/prod(dim(resultDiffProb))
    accuracy <- mean(testLabelRaw == predictionsMax)
    
    confusionMatrix <- matrix(table(factor(predictionsMax, 
                                           levels =  levels(testLabelRaw)),
                                    factor(testLabelRaw, 
                                           levels =  levels(testLabelRaw))), 
                              ncol = length(levels(testLabelRaw)))
    
    confAcc <- sum(diag(confusionMatrix)) / sum(confusionMatrix)
    
    # checking accuracy of model and from confusion matrix
    print(paste("model accuracy is", accuracy,
                "and confusion matrix accuracy is", confAcc))
    
    
    # naming rows and cols
    rownames(confusionMatrix) <- levels(testLabelRaw)
    colnames(confusionMatrix) <- levels(testLabelRaw)
    print(paste("sum of confusionMatrix is ", sum(confusionMatrix)))
    
    # accuracy by class
    accByClass <- diag(as.matrix(confusionMatrix)) / colSums(truth)
    names(accByClass) <-  levels(testLabelRaw)
    
  }
  
  
  return(list(acc = accuracy,
              meanSquareError = meanSquareError,
              confusionMatrix = confusionMatrix,
              ProbAccDT = ProbAccDT,
              accByClass = accByClass,
              predictions = predictions,
              testLabelRaw = testLabelRaw))
  
}


predictCNN <- function(dataPath, fileName, indexName, epochs = 12) {
  assertString(dataPath)
  assertString(fileName)
  assertString(indexName)

 
  print("read in Data")
  dataRDS <- readRDS(paste0(dataPath, fileName))
  data <- dataRDS[["wordVectorArray"]]
  label <- as.factor(dataRDS[["label"]])
  maxWords <- dataRDS[["maxWords"]]
  channels <- dataRDS[["channels"]]
  indexes <- read.fst(paste0(dataPath, indexName), as.data.table = TRUE)[[1]] 
  
  trainData <- data[indexes, , ]
  testData <- data[-indexes, , ]
  
  trainLabelRaw <- label[indexes]
  testLabelRaw <- label[-indexes]
  
  print(paste("in train are number of uniques:", 
              length(unique(trainLabelRaw))))
  print(paste("in test are number of uniques:", 
              length(unique(testLabelRaw))))
  
  # setting up trainLabel
  trainLabelNumeric <- as.numeric(trainLabelRaw) - 1
  names(trainLabelNumeric) <- trainLabelRaw
  trainLabel <- to_categorical(trainLabelNumeric)
  
  # setting up testLabel
  testLabelNumeric <- as.numeric(testLabelRaw) - 1
  names(testLabelNumeric) <- testLabelRaw
  testLabel <- to_categorical(testLabelNumeric)
  
  # building model with layers
  model <- keras_model_sequential()
  model %>% 
    # Add a Convolution1D, which will learn filters
    # Word group filters of size filter_length:
    layer_conv_1d(input_shape  = list(maxWords, channels),
                  data_format = "channels_last",
      filters = 50, kernel_size = 2, 
      padding = "same", activation = "relu", strides = 1,
      name = "conv1"#, trainable = FALSE
    ) %>%
    layer_conv_1d(filters = 100, kernel_size = 3,
                  padding = "same", activation = "relu",
                  strides = 1,
                  name = "conv2") %>%
    layer_conv_1d(filters = 100, kernel_size = 4,
                  padding = "same", activation = "relu",
                  strides = 1,
                  name = "conv3") %>%
    layer_conv_1d(filters = 100, kernel_size = 5,
                  padding = "same", activation = "relu",
                  strides = 1,
                  name = "conv4") %>%
    layer_global_max_pooling_1d() %>%
    
    # Add a vanilla hidden layer:
    layer_dense(100) %>%
    
    # Apply 20% layer dropout
    layer_dropout(0.2) %>%
    layer_activation("relu") %>%
    
    # Project onto a single unit output layer, and squash it with a sigmoid
    layer_dense(units = ncol(trainLabel),
                activation = "softmax", 
                name = "predictions")

  # Compiling model
  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_adam(lr = 0.001),
    metrics = c('accuracy')
  )

  print("fitting model")
  
  history <- model %>% fit(
    x = trainData,
    y = trainLabel,
    epochs = epochs,
    batchsize = 32,
    validation_data = list(testData, testLabel),
    view_metrics = FALSE,
    verbose = 2)
  
  evaluationResult <- model %>% 
    evaluate(testData, testLabel, batch_size = 32)
  
  predictProb <- data.table(model %>% 
                              predict(testData, testLabel, batch_size = 32))
  names(predictProb) <- levels(trainLabelRaw)
  # see for which class the prop is the highest
  predictMax <- t(apply(predictProb, 1, function(x) {
    return(names(x[which.max(x)]))
  }))
  levels(predictMax) <- levels(testLabelRaw)
  confusionMatrix <- matrix(table(factor(predictMax, 
                                         levels =  levels(testLabelRaw)),
                                  factor(testLabelRaw, 
                                         levels =  levels(testLabelRaw))), 
                            ncol = length(levels(testLabelRaw)))

  confAcc <- sum(diag(confusionMatrix)) / sum(confusionMatrix)
  
  # checking accuracy of model and from confusion matrix
  print(paste("model accuracy is", evaluationResult$acc,
              "and confusion matrix accuracy is", confAcc))
  
  # naming rows and cols
  rownames(confusionMatrix) <- levels(testLabelRaw)
  colnames(confusionMatrix) <- levels(testLabelRaw)
  
  print(paste("sum of confusionMatrix is ", sum(confusionMatrix)))
  
  # Prob vs Accuracy plot Data
  predictionsMaxProb <- as.vector(t(apply(predictProb, 1 ,
                                          function(x) {
                                            x[which.max(x)]
                                          })))
  correctBinary <- testLabelRaw == as.vector(predictMax)
  ProbAccDT <- data.table(Prob = predictionsMaxProb,
                          Correct = correctBinary)
  
  
  
  accByClass <- diag(as.matrix(confusionMatrix)) / colSums(testLabel)
  names(accByClass) <-  levels(trainLabelRaw)
  
  
  return(list(acc = evaluationResult$acc,
              loss = evaluationResult$loss,
              confusionMatrix = confusionMatrix,
              accByClass = accByClass,
              predictions = predictProb,
              testLabelRaw = testLabelRaw))
}



predictEmb <- function(dataPath, fileName, indexName,
                       upSampling = FALSE, epochs = 12) {
  assertString(dataPath)
  assertString(fileName)
  assertString(indexName)
  assertFlag(upSampling)
  
  print("read in Data")
  data <- read.fst(paste0(dataPath, fileName), as.data.table = TRUE)

  label <- data$labelRaw

  indexes <- read.fst(paste0(dataPath, indexName), as.data.table = TRUE)[[1]]  
  

  trainData <- data[indexes, , ]
  if(upSampling) trainData <- generalizedSampling(data = trainData, 
                                                  method = "up", 
                                             label = "labelRaw")
  trainLabelRaw <- trainData$labelRaw
  testData <- data[-indexes, , ]
  
  trainData[, labelRaw := NULL]
  testData[, labelRaw := NULL]
  
  testLabelRaw <- label[-indexes]
  
  print(paste("in train are number of uniques:", 
              length(unique(trainLabelRaw))))
  print(paste("in test are number of uniques:", 
              length(unique(testLabelRaw))))
  
  # setting up trainLabel
  trainLabelNumeric <- as.numeric(trainLabelRaw) - 1
  names(trainLabelNumeric) <- trainLabelRaw
  trainLabel <- to_categorical(trainLabelNumeric)

  # setting up testLabel
  testLabelNumeric <- as.numeric(testLabelRaw) - 1
  names(testLabelNumeric) <- testLabelRaw
  testLabel <- to_categorical(testLabelNumeric)
  
  nVocab = max(rbind(trainData,testData)) + 1
  
  model <- keras_model_sequential() %>% 
    # Start off with an efficient embedding layer which maps
    # the vocab indices into embedding_dims dimensions
    layer_embedding(input_dim = nVocab,
                    output_dim = 50, 
                    input_length = ncol(trainData)) %>%
    layer_dropout(0.2) %>%
    
    # Add a Convolution1D, which will learn filters
    layer_conv_1d(filters = 100, kernel_size  = 2, 
      padding = "valid", activation = "relu", strides = 1,
      name = "conv1") %>%
    layer_dropout(0.1) %>%
    layer_conv_1d(filters = 100, kernel_size = 3,
                  padding = "same", activation = "relu",
                  strides = 1,
                  name = "conv2") %>%
    # layer_dropout(0.1) %>%
    # layer_conv_1d(filters = 100, kernel_size = 4,
    #               padding = "same", activation = "relu",
    #               strides = 1,
    #               name = "conv3") %>%
    # layer_dropout(0.1) %>%
    # layer_conv_1d(filters = 100, kernel_size = 5,
    #               padding = "same", activation = "relu",
    #               strides = 1,
    #               name = "conv4") %>%
    layer_dropout(0.1) %>%
    # Apply max pooling:
    layer_global_max_pooling_1d() %>%
    
    # Add a vanilla hidden layer:
    layer_dense(units = 100) %>%
    
    # Apply 20% layer dropout
    layer_dropout(0.2) %>%
    layer_activation("relu") %>%
    
    # Project onto a single unit output layer, and squash it with a sigmoid
    layer_dense(units = ncol(trainLabel),
                activation = "softmax", 
                name = "predictions")
  
 
  # Compiling model
  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(lr = 0.0005),
    metrics = c('accuracy')
  )
  
  print("fitting model")
  
  history <- model %>% fit(
    x = as.matrix(trainData),
    y = trainLabel,
    epochs = epochs,
    batchsize = 32,
    validation_data = list(as.matrix(testData), testLabel),
    view_metrics = FALSE,
    verbose = 2)
  
  evaluationResult <- model %>% 
    evaluate(as.matrix(testData), testLabel, batch_size = 32)
  
  predictProb <- data.table(model %>% 
    predict(as.matrix(testData), testLabel, batch_size = 32))
  names(predictProb) <- levels(trainLabelRaw)
  # see for which class the prop is the highest
  predictMax <- t(apply(predictProb, 1, function(x) {
    return(names(x[which.max(x)]))
  }))
  
  # Prob vs Accuracy plot Data
  predictionsMaxProb <- as.vector(t(apply(predictProb, 1 ,
                                          function(x) {
                                            x[which.max(x)]
                                          })))
  correctBinary <- testLabelRaw == as.vector(predictMax)
  ProbAccDT <- data.table(Prob = predictionsMaxProb,
                          Correct = correctBinary)
  
  levels(predictMax) <- levels(testLabelRaw)
  confusionMatrix <- matrix(table(factor(predictMax, 
                                  levels =  levels(testLabelRaw)),
                           factor(testLabelRaw, 
                                  levels =  levels(testLabelRaw))),
                           ncol = length(levels(testLabelRaw)))
  
  confAcc <- sum(diag(confusionMatrix)) / sum(confusionMatrix)
  
  # checking accuracy of model and from confusion matrix
  print(paste("model accuracy is", evaluationResult$acc,
              "and confusion matrix accuracy is", confAcc))
  
  # naming rows and cols
  rownames(confusionMatrix) <- levels(testLabelRaw)
  colnames(confusionMatrix) <- levels(testLabelRaw)

    print(paste("sum of confusionMatrix is ", sum(confusionMatrix)))
        
  accByClass <- diag(as.matrix(confusionMatrix)) / colSums(testLabel)
  names(accByClass) <-  levels(trainLabelRaw)

  
  return(list(acc = evaluationResult$acc,
              loss = evaluationResult$loss,
              confusionMatrix = confusionMatrix,
              ProbAccDT = ProbAccDT,
              accByClass = accByClass,
              predictions = predictProb,
              testLabelRaw = testLabelRaw))
}


predictLSTM <- function(dataPath, fileName, indexName,
                        upSampling = FALSE, epochs = 12) {
  assertString(dataPath)
  assertString(fileName)
  assertString(indexName)
  
  print("read in Data")
  data <- read.fst(paste0(dataPath, fileName), as.data.table = TRUE)
  label <- data$labelRaw
  
  indexes <- read.fst(paste0(dataPath, indexName), as.data.table = TRUE)[[1]]  
  
  trainData <- data[indexes, , ]
  if(upSampling) trainData <- generalizedSampling(data = trainData, 
                                                  method = "up", 
                                                  label = "labelRaw")
  trainLabelRaw <- trainData$labelRaw
  
  testData <- data[-indexes, , ]
  
  trainData[, labelRaw := NULL]
  testData[, labelRaw := NULL]
  
  testLabelRaw <- label[-indexes]
  
  print(paste("in train are number of uniques:", 
              length(unique(trainLabelRaw))))
  print(paste("in test are number of uniques:", 
              length(unique(testLabelRaw))))
  
  # setting up trainLabel
  trainLabelNumeric <- as.numeric(trainLabelRaw) - 1
  names(trainLabelNumeric) <- trainLabelRaw
  trainLabel <- to_categorical(trainLabelNumeric)
  
  # setting up testLabel
  testLabelNumeric <- as.numeric(testLabelRaw) - 1
  names(testLabelNumeric) <- testLabelRaw
  testLabel <- to_categorical(testLabelNumeric)
  
  nVocab = max(rbind(trainData,testData)) + 1
  
  model <- keras_model_sequential() %>% 
    # Start off with an efficient embedding layer which maps
    # the vocab indices into embedding_dims dimensions
    layer_embedding(input_dim = nVocab,
                    output_dim = 50, 
                    input_length = ncol(trainData)) %>%
    bidirectional(layer_lstm(units = 128)) %>%
    layer_dropout(rate = 0.4) %>% 

    
    # Add a vanilla hidden layer:
    layer_dense(units = 100) %>%
    
    # Apply 20% layer dropout
    layer_dropout(0.2) %>%
    layer_activation("relu") %>%
    
    # Project onto a single unit output layer, and squash it with a sigmoid
    layer_dense(units = ncol(trainLabel),
                activation = "softmax", 
                name = "predictions")
  
  # Compiling model
  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(lr = 0.001),
    metrics = c('accuracy')
  )
  
  print("fitting model")
  
  history <- model %>% fit(
    x = as.matrix(trainData),
    y = trainLabel,
    epochs = epochs,
    batchsize = 32,
    validation_data = list(as.matrix(testData), testLabel),
    view_metrics = FALSE,
    verbose = 2)
  
  evaluationResult <- model %>% 
    evaluate(as.matrix(testData), testLabel, batch_size = 32)
  
  predictProb <- data.table(model %>% 
                              predict(as.matrix(testData), testLabel, batch_size = 32))
  names(predictProb) <- levels(trainLabelRaw)
  # see for which class the prop is the highest
  predictMax <- t(apply(predictProb, 1, function(x) {
    return(names(x[which.max(x)]))
  }))
  
  levels(predictMax) <- levels(testLabelRaw)
  confusionMatrix <- matrix(table(factor(predictMax, 
                                         levels =  levels(testLabelRaw)),
                                  factor(testLabelRaw, 
                                         levels =  levels(testLabelRaw))),
                            ncol = length(levels(testLabelRaw)))
  
  confAcc <- sum(diag(confusionMatrix)) / sum(confusionMatrix)
  
  # checking accuracy of model and from confusion matrix
  print(paste("model accuracy is", evaluationResult$acc,
              "and confusion matrix accuracy is", confAcc))
  
  # naming rows and cols
  rownames(confusionMatrix) <- levels(testLabelRaw)
  colnames(confusionMatrix) <- levels(testLabelRaw)
  
  print(paste("sum of confusionMatrix is ", sum(confusionMatrix)))
  
  
  # Prob vs Accuracy plot Data
  predictionsMaxProb <- as.vector(t(apply(predictProb, 1 ,
                                          function(x) {
                                            x[which.max(x)]
                                          })))
  correctBinary <- testLabelRaw == as.vector(predictMax)
  ProbAccDT <- data.table(Prob = predictionsMaxProb,
                          Correct = correctBinary)
  
  
  # accuracy by class
  accByClass <- diag(as.matrix(confusionMatrix)) / colSums(testLabel)
  names(accByClass) <-  levels(trainLabelRaw)
  
  
  return(list(acc = evaluationResult$acc,
              loss = evaluationResult$loss,
              confusionMatrix = confusionMatrix,
              ProbAccDT = ProbAccDT,
              accByClass = accByClass,
              predictions = predictProb,
              testLabelRaw = testLabelRaw))
}

predictLSTMArray <- function(dataPath, fileName, indexName,
                             upSampling = FALSE, epochs = 12) {
  assertString(dataPath)
  assertString(fileName)
  assertString(indexName)
  
  print("read in Data")
  dataRDS <- readRDS(paste0(dataPath, fileName))
  data <- dataRDS[["wordVectorArray"]]
  label <- as.factor(dataRDS[["label"]])
  maxWords <- dataRDS[["maxWords"]]
  channels <- dataRDS[["channels"]]
  indexes <- read.fst(paste0(dataPath, indexName), as.data.table = TRUE)[[1]] 
  
  rm(dataRDS)
  gc()
  
  trainData <- data[indexes, , ]
  testData <- data[-indexes, , ]
  
  rm(data)
  gc()
  
  if(upSampling) print("sampling does not work for 3dim arrays, 
                       continue without")
  
  trainLabelRaw <- label[indexes]
  testLabelRaw <- label[-indexes]
  
  print(paste("in train are number of uniques:", 
              length(unique(trainLabelRaw))))
  print(paste("in test are number of uniques:", 
              length(unique(testLabelRaw))))
  
  print("setting up train and test Label")
  trainLabelNumeric <- as.numeric(trainLabelRaw) - 1
  names(trainLabelNumeric) <- trainLabelRaw
  trainLabel <- to_categorical(trainLabelNumeric)
  

  testLabelNumeric <- as.numeric(testLabelRaw) - 1
  names(testLabelNumeric) <- testLabelRaw
  testLabel <- to_categorical(testLabelNumeric)
  
  model <- keras_model_sequential() %>%
    # Add a Convolution1D, which will learn filters
    # Word group filters of size filter_length:
    layer_conv_1d(input_shape  = list(maxWords, channels),
                  data_format = "channels_last",
                  filters = 50, kernel_size = 2, 
                  padding = "same", activation = "relu", strides = 1,
                  name = "conv1"#, trainable = FALSE
    ) %>%
    bidirectional(layer_lstm(units = 128)) %>%
    layer_dropout(rate = 0.4) %>% 
    
    # Add a vanilla hidden layer:
    layer_dense(units = 100) %>%
    
    # Apply 20% layer dropout
    layer_dropout(0.2) %>%
    layer_activation("relu") %>%
    
    # Project onto a single unit output layer, and squash it with a sigmoid
    layer_dense(units = ncol(trainLabel),
                activation = "softmax", 
                name = "predictions")
  # Compiling model
  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_adam(lr = 0.001),
    metrics = c('accuracy')
  )
  
  print("fitting model")
  
  history <- model %>% fit(
    x = trainData,
    y = trainLabel,
    epochs = epochs,
    batchsize = 32,
    validation_data = list(testData, testLabel),
    view_metrics = FALSE,
    verbose = 2)
  
  evaluationResult <- model %>% 
    evaluate(testData, testLabel, batch_size = 32)
  
  predictProb <- data.table(model %>% 
                              predict(testData, testLabel, batch_size = 32))
  
 
  names(predictProb) <- levels(trainLabelRaw)
  # see for which class the prop is the highest
  predictMax <- t(apply(predictProb, 1, function(x) {
    return(names(x[which.max(x)]))
  }))
  levels(predictMax) <- levels(testLabelRaw)
  confusionMatrix <- matrix(table(factor(predictMax, 
                                         levels =  levels(testLabelRaw)),
                                  factor(testLabelRaw, 
                                         levels =  levels(testLabelRaw))),
                            ncol = length(levels(testLabelRaw)))
 
  confAcc <- sum(diag(confusionMatrix)) / sum(confusionMatrix)
  
  # checking accuracy of model and from confusion matrix
  print(paste("model accuracy is", evaluationResult$acc,
              "and confusion matrix accuracy is", confAcc))
  
  # Prob vs Accuracy plot Data
  predictionsMaxProb <- as.vector(t(apply(predictProb, 1 ,
                                          function(x) {
                                            x[which.max(x)]
                                          })))
  correctBinary <- testLabelRaw == as.vector(predictMax)
  ProbAccDT <- data.table(Prob = predictionsMaxProb,
                          Correct = correctBinary)
  
  # naming rows and cols
  rownames(confusionMatrix) <- levels(testLabelRaw)
  colnames(confusionMatrix) <- levels(testLabelRaw)
  
  print(paste("sum of confusionMatrix is ", sum(confusionMatrix)))
  
  accByClass <- diag(as.matrix(confusionMatrix)) / colSums(testLabel)
  names(accByClass) <-  levels(trainLabelRaw)
  
  
  return(list(acc = evaluationResult$acc,
              loss = evaluationResult$loss,
              confusionMatrix = confusionMatrix,
              ProbAccDT = ProbAccDT,
              accByClass = accByClass,
              predictions = predictProb,
              testLabelRaw = testLabelRaw))
}




ensembleMaxProb <- function(..., truth){
  input <- list(...)
  allProbs <- as.data.table(do.call(cbind, input))
  
  predictions <- apply(allProbs, 1, function(x) {
    return(names(x[which.max(x)]))
  })
  if (length(truth) != length(predictions)) stop("different vector lengths")
  accuracy <- mean(predictions == truth)
  
  # todo: fix hardcoding here
  confusionMatrix <- matrix(table(factor(predictions, levels = levels(truth)),
                           factor(truth, levels = levels(truth))), 
                           ncol = 35)
  row.names(confusionMatrix) <- levels(truth)
  
 return(list(acc = accuracy,
             confusionMatrix = confusionMatrix))
}

