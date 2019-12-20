
predictMLP <- function(dataPath, fileName, 
                       upSampling = FALSE, epochs = 12) {
  assertString(dataPath)
  assertString(fileName)
  assertFlag(upSampling)
  
  print("read in Data")
  data <- readRDS(paste0(dataPath, fileName))
  assertList(data)
  assertSubset(names(data), choices = c("resultTrain", "resultTest",
                                        "labelTrain", "labelTest",
                                        "IndexDT", "maxWords", "channels"))
  
  trainData <- data$resultTrain
  if(upSampling) trainData <- generalizedSampling(data = trainData, 
                                                  method = "up", 
                                                  label = "labelRaw")
  testData <- data$resultTest
  trainLabelRaw <- data$labelTrain
  assertFactor(trainLabelRaw)
  testLabelRaw <- data$labelTest
  assertFactor(testLabelRaw)
  
  rm(data)  # praembel end
  
  assert(length(unique(trainLabelRaw)) == length(unique(testLabelRaw)))
  
  # setting up trainLabel
  trainLabelNumeric <- as.numeric(trainLabelRaw) - 1
  names(trainLabelNumeric) <-trainLabelRaw
  trainLabel <- to_categorical(trainLabelNumeric)
  
  # setting up testLabel
  testLabelNumeric <- as.numeric(testLabelRaw) - 1
  names(testLabelNumeric) <- testLabelRaw
  testLabel <- to_categorical(testLabelNumeric)
  
 
  # nVocab = max(rbind(trainData,testData)) + 1
  model <- keras_model_sequential() %>% 
    
    # Add a vanilla hidden layer:
    layer_dense(units = 100) %>%
    # Apply 20% layer dropout
    layer_dropout(0.2) %>%
    
    # Add a vanilla hidden layer:
    layer_dense(units = 100) %>%
    # Apply 20% layer dropout
    layer_dropout(0.2) %>%
    
    # Add a vanilla hidden layer:
    layer_dense(units = 100) %>%
    
    layer_batch_normalization() %>%
    
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
  confusionMatrix <- t(matrix(table(factor(predictMax, 
                                         levels =  levels(testLabelRaw)),
                                  factor(testLabelRaw, 
                                         levels =  levels(testLabelRaw))),
                            ncol = length(levels(testLabelRaw))))
  
  confAcc <- sum(diag(confusionMatrix)) / sum(confusionMatrix)
  
  # checking accuracy of model and from confusion matrix
  print(paste("model accuracy is", evaluationResult$acc,
              "and confusion matrix accuracy is", confAcc))
  
  # naming rows and cols
  rownames(confusionMatrix) <- levels(testLabelRaw)
  colnames(confusionMatrix) <- levels(testLabelRaw)
  
  
  print(paste("sum of confusionMatrix is ", sum(confusionMatrix)))
  
  accByClass <- diag(as.matrix(confusionMatrix)) / colSums(testLabel)
  assert(all(accByClass <= 1))
  names(accByClass) <-  levels(trainLabelRaw)
  
  
  return(list(acc = evaluationResult$acc,
              loss = evaluationResult$loss,
              confusionMatrix = confusionMatrix,
              ProbAccDT = ProbAccDT,
              accByClass = accByClass,
              predictions = predictProb,
              testLabelRaw = testLabelRaw))
}



predictLogReg <- function(dataPath, fileName,  
                      upSampling = FALSE, sparse = FALSE){
  assertString(dataPath)
  assertString(fileName)
  assertFlag(upSampling)
  
  print("read in Data")
  data <- readRDS(paste0(dataPath, fileName))
  assertList(data)
  assertSubset(names(data), choices = c("resultTrain", "resultTest",
                                        "labelTrain", "labelTest",
                                        "IndexDT", "maxWords", "channels"))
  
  trainData <- data$resultTrain
  if(upSampling) trainData <- generalizedSampling(data = trainData, 
                                                  method = "up", 
                                                  label = "labelRaw")
  testData <- data$resultTest
  trainLabelRaw <- data$labelTrain
  assertFactor(trainLabelRaw)
  testLabelRaw <- data$labelTest
  assertFactor(testLabelRaw)
  
  rm(data) # praembel end
  
  trainLabel <- as.numeric(trainLabelRaw) - 1
  testLabel <- as.numeric(testLabelRaw) - 1
  
  numClass <- length(unique(trainLabel))

  assert(length(unique(trainLabelRaw)) == length(unique(testLabelRaw)))
  
  print("training logreg model")
  model <- LiblineaR::LiblineaR(trainData, target =  trainLabel,
                                type = 0)
  
  # evaluating predictions and eval metrics
  predictions <- as.data.table(predict(model, 
                                       newx =  testData,
                                       proba = TRUE))
  # neu ordern
  predictions[, predictions := NULL]
  
  
  # extract the order of the columns in predictions
  orderOfNames <- as.integer(gsub(colnames(predictions),
                                  pattern = "probabilities.",
                                  replacement = "")) + 1
  
  # evaluating predictions and eval metrics
  names(predictions) <- levels(testLabelRaw)[orderOfNames]
  
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
  
  confusionMatrix <- t(matrix(table(factor(predictionsMax, 
                                         levels =  levels(testLabelRaw)),
                                  factor(testLabelRaw, 
                                         levels =  levels(testLabelRaw))), 
                            ncol = numClass))
  
  confAcc <- sum(diag(confusionMatrix)) / sum(confusionMatrix)
  
  # checking accuracy of model and from confusion matrix
  print(paste("model accuracy is", accuracy,
              "and confusion matrix accuracy is", confAcc))
  
  # naming rows and cols
  rownames(confusionMatrix) <- levels(testLabelRaw)
  colnames(confusionMatrix) <- levels(testLabelRaw)
  print(paste("sum of confusionMatrix is ", sum(confusionMatrix)))
  
  # accuracy by class: colSums are the amount of true labels
  accByClass <- diag(confusionMatrix) / colSums(confusionMatrix)
  assert(all(accByClass <= 1))
  names(accByClass) <-  levels(testLabelRaw)
  
  
  return(list(acc = accuracy,
              meanSquareError = meanSquareError,
              confusionMatrix = confusionMatrix,
              ProbAccDT = ProbAccDT,
              accByClass = accByClass,
              predictions = predictions,
              testLabelRaw = testLabelRaw))
  
}

predictNB <- function(dataPath, fileName,  
                          upSampling = FALSE, sparse = FALSE){
  assertString(dataPath)
  assertString(fileName)
  assertFlag(upSampling)
  
  print("read in Data")
  data <- readRDS(paste0(dataPath, fileName))
  assertList(data)
  assertSubset(names(data), choices = c("resultTrain", "resultTest",
                                        "labelTrain", "labelTest",
                                        "IndexDT", "maxWords", "channels"))
  
  trainData <- data$resultTrain
  if(upSampling) trainData <- generalizedSampling(data = trainData, 
                                                  method = "up", 
                                                  label = "labelRaw")
  testData <- data$resultTest
  trainLabelRaw <- data$labelTrain
  assertFactor(trainLabelRaw)
  testLabelRaw <- data$labelTest
  assertFactor(testLabelRaw)
  
  rm(data) # praembel end
  trainLabel <- as.numeric(trainLabelRaw) - 1
  testLabel <- as.numeric(testLabelRaw) - 1
  
  numClass <- length(unique(trainLabel))
  
  assert(length(unique(trainLabelRaw)) == length(unique(testLabelRaw)))
  
  print("training naive bayes model")
  model <- naive_bayes(x = trainData, y = trainLabelRaw,
                       laplace = 1, usekernel = TRUE,
                       usepoisson = TRUE)

  # evaluating predictions and eval metrics
  predictions <- as.data.table(predict(model, 
                                       newdata =  testData,
                                       type = "prob"))
  
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
  
  confusionMatrix <- t(matrix(table(factor(predictionsMax, 
                                         levels =  levels(testLabelRaw)),
                                  factor(testLabelRaw, 
                                         levels =  levels(testLabelRaw))), 
                            ncol = numClass))
  
  confAcc <- sum(diag(confusionMatrix)) / sum(confusionMatrix)
  
  # checking accuracy of model and from confusion matrix
  print(paste("model accuracy is", accuracy,
              "and confusion matrix accuracy is", confAcc))
  
  # naming rows and cols
  rownames(confusionMatrix) <- levels(testLabelRaw)
  colnames(confusionMatrix) <- levels(testLabelRaw)
  print(paste("sum of confusionMatrix is ", sum(confusionMatrix)))
  
  # accuracy by class: colSums are the amount of true labels
  accByClass <- diag(confusionMatrix) / colSums(confusionMatrix)
  assert(all(accByClass <= 1))
  names(accByClass) <-  levels(testLabelRaw)
  
  
  return(list(acc = accuracy,
              meanSquareError = meanSquareError,
              confusionMatrix = confusionMatrix,
              ProbAccDT = ProbAccDT,
              accByClass = accByClass,
              predictions = predictions,
              testLabelRaw = testLabelRaw))
  
}



predictXG <- function(dataPath, fileName,  
                      upSampling = FALSE, sparse = FALSE, nrounds = 20){
  assertString(dataPath)
  assertString(fileName)
  assertFlag(upSampling)
  

  print("read in Data")
  data <- readRDS(paste0(dataPath, fileName))
  assertList(data)
  assertSubset(names(data), choices = c("resultTrain", "resultTest",
                                        "labelTrain", "labelTest",
                                        "IndexDT", "maxWords", "channels"))
  
  trainData <- data$resultTrain
  if(upSampling) trainData <- generalizedSampling(data = trainData, 
                                                  method = "up", 
                                                  label = "labelRaw")
  testData <- data$resultTest
  trainLabelRaw <- data$labelTrain
  assertFactor(trainLabelRaw)
  testLabelRaw <- data$labelTest
  assertFactor(testLabelRaw)
  
  rm(data)  

  assert(length(unique(trainLabelRaw)) == length(unique(testLabelRaw)))
  
  numClass <-  length(unique(trainLabelRaw))
  
  trainLabel <- as.numeric(trainLabelRaw) - 1
  testLabel <- as.numeric(testLabelRaw) - 1
  # praembel end  

  if(!sparse) {
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
    # create watchlist
    watchIndexes <- sample.int(nrow(trainData), 
                               size = round(nrow(trainData) * 0.8))
    watchTrain <- trainData[watchIndexes, ]
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
  eval_metric <- c("mlogloss", "merror")
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
  
  confusionMatrix <- t(matrix(table(factor(predictionsMax, 
                                         levels =  levels(testLabelRaw)),
                                  factor(testLabelRaw, 
                                         levels =  levels(testLabelRaw))), 
                            ncol = numClass))
  
  confAcc <- sum(diag(confusionMatrix)) / sum(confusionMatrix)
  
  # checking accuracy of model and from confusion matrix
  print(paste("model accuracy is", accuracy,
              "and confusion matrix accuracy is", confAcc))
  
  # naming rows and cols
  rownames(confusionMatrix) <- levels(testLabelRaw)
  colnames(confusionMatrix) <- levels(testLabelRaw)
  print(paste("sum of confusionMatrix is ", sum(confusionMatrix)))
  
  # accuracy by class: colSums are the amount of true labels
  accByClass <- diag(confusionMatrix) / colSums(confusionMatrix)
  assert(all(accByClass <= 1))
  names(accByClass) <-  levels(testLabelRaw)
  
  
  return(list(acc = accuracy,
              meanSquareError = meanSquareError,
              confusionMatrix = confusionMatrix,
              ProbAccDT = ProbAccDT,
              accByClass = accByClass,
              predictions = predictions,
              testLabelRaw = testLabelRaw))

}


predictRF <- function(dataPath, fileName,  labelName = NULL,
                      upSampling = FALSE, sparse = FALSE, num.trees = 500) {
  assertString(dataPath)
  assertString(fileName)
  assertFlag(upSampling)
  

  print("read in Data")
  data <- readRDS(paste0(dataPath, fileName))
  assertList(data)
  assertSubset(names(data), choices = c("resultTrain", "resultTest",
                                        "labelTrain", "labelTest",
                                        "IndexDT", "maxWords", "channels"))
  
  trainData <- data$resultTrain
  if(upSampling) trainData <- generalizedSampling(data = trainData, 
                                                  method = "up", 
                                                  label = "labelRaw")
  testData <- data$resultTest
  trainLabelRaw <- data$labelTrain
  assertFactor(trainLabelRaw)
  testLabelRaw <- data$labelTest
  assertFactor(testLabelRaw)
  
  rm(data)  
  
  assert(length(unique(trainLabelRaw)) == length(unique(testLabelRaw)))
  
  numClass <-  length(unique(trainLabelRaw))
  
  trainLabel <- as.numeric(trainLabelRaw) - 1
  testLabel <- as.numeric(testLabelRaw) - 1
  # praembel end  
  
  if(!sparse){
    trainData <- data.table(labelRaw = trainLabel, trainData)
    print("fitting model with data.table")
    model = ranger::ranger(dependent.variable.name = "labelRaw", 
                           data = trainData,
                           importance = "impurity",
                           probability = TRUE, num.trees = num.trees,
                           save.memory = TRUE)
    
    predictions = as.data.table(predict(model, 
                                        testData, 
                                        type = "response")$predictions)
    
    orderOfNames <- as.integer(colnames(model$predictions)) + 1
    # evaluating predictions and eval metrics
    names(predictions) <- levels(testLabelRaw)[orderOfNames]
    
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
    
    confusionMatrix <- t(matrix(table(factor(predictionsMax, 
                                           levels =  levels(testLabelRaw)),
                                    factor(testLabelRaw, 
                                           levels =  levels(testLabelRaw))), 
                              ncol = length(levels(testLabelRaw))))
    
    confAcc <- sum(diag(confusionMatrix)) / sum(confusionMatrix)
    
    # checking accuracy of model and from confusion matrix
    print(paste("model accuracy is", accuracy,
                "and confusion matrix accuracy is", confAcc))
    
    
    # naming rows and cols
    rownames(confusionMatrix) <- levels(testLabelRaw)
    colnames(confusionMatrix) <- levels(testLabelRaw)
    print(paste("sum of confusionMatrix is ", sum(confusionMatrix)))
    
    # accuracy by class: colSums are the amount of true labels
    accByClass <- diag(confusionMatrix) / colSums(confusionMatrix)
    assert(all(accByClass <= 1))
    names(accByClass) <-  levels(testLabelRaw)
    
  }
  
  if (sparse){
    # quickfixes for new tfidf method, dfm is not supported by ranger
    
    if (class(trainData) != "dgCMatrix") {
      # cleaning NA values
      train_x <- trainData@x
      train_x[is.na(train_x)] <- 0
      trainData@x <- train_x
      
      test_x <- testData@x
      test_x[is.na(test_x)] <- 0
      testData@x <- test_x
      
      trainData <- as(trainData, "dgCMatrix")
      testData <- as(testData, "dgCMatrix")
      
      assert(!any(is.na(trainData@x)))
      assert(!any(is.na(testData@x)))
    } else {
      # cleaning NA values
      train_x <- trainData@x
      train_x[is.na(train_x)] <- 0
      trainData@x <- train_x
      
      test_x <- testData@x
      test_x[is.na(test_x)] <- 0
      testData@x <- test_x
    }


    # because of naming transition use not "-1"
    trainLabel <- Matrix::Matrix(data = as.numeric(trainLabelRaw),
                                 sparse = TRUE, ncol = 1)
    #trainLabel <- as.dfm(trainLabel)
    dimnames(trainLabel)[[2]] <- "trainLabel"
    testLabel <- as.numeric(testLabelRaw)
    
    trainSparse <- cbind(trainLabel, trainData)
    testSparse <- cbind(testLabel, testData)

    print("fitting model with sparse")
    model = ranger::ranger(dependent.variable.name = "trainLabel", 
                           data = trainSparse,
                           importance = "impurity",
                           probability = TRUE, num.trees = num.trees,
                           classification = TRUE, verbose = TRUE,
                           save.memory = TRUE)
    # when calc. with a sparse matrix, the column names are ordered along
    # the true classes of the first training examples
    orderOfNames <- as.integer(colnames(model$predictions))
    
    predictions = as.data.table(predict(model, 
                                        testData, 
                                        type = "response")$predictions)
    
    # evaluating predictions and eval metrics
    names(predictions) <- levels(testLabelRaw)[orderOfNames]
    
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
    
    confusionMatrix <- t(matrix(table(factor(predictionsMax, 
                                           levels =  levels(testLabelRaw)),
                                    factor(testLabelRaw, 
                                           levels =  levels(testLabelRaw))), 
                              ncol = length(levels(testLabelRaw))))
    
    confAcc <- sum(diag(confusionMatrix)) / sum(confusionMatrix)
    
    # checking accuracy of model and from confusion matrix
    print(paste("model accuracy is", accuracy,
                "and confusion matrix accuracy is", confAcc))
    
    
    # naming rows and cols
    rownames(confusionMatrix) <- levels(testLabelRaw)
    colnames(confusionMatrix) <- levels(testLabelRaw)
    print(paste("sum of confusionMatrix is ", sum(confusionMatrix)))
    
    # accuracy by class: colSums are the amount of true labels
    accByClass <- diag(confusionMatrix) / colSums(confusionMatrix)
    assert(all(accByClass <= 1))
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


predictCNNArray <- function(dataPath, fileName, 
                            upSampling = FALSE, epochs = 12) {
  assertString(dataPath)
  assertString(fileName)
  assertNumber(epochs)
  
  print("read in Data")
  data <- readRDS(paste0(dataPath, fileName))
  assertList(data)
  assertSubset(names(data), choices = c("resultTrain", "resultTest",
                                        "labelTrain", "labelTest",
                                        "IndexDT", "maxWords", "channels"))
  
  trainData <- data$resultTrain
  if(upSampling) trainData <- generalizedSampling(data = trainData, 
                                                  method = "up", 
                                                  label = "labelRaw")
  testData <- data$resultTest
  trainLabelRaw <- data$labelTrain
  assertFactor(trainLabelRaw)
  testLabelRaw <- data$labelTest
  assertFactor(testLabelRaw)
  
  maxWords <- data$maxWords
  channels <- data$channels
  rm(data)  
  
  assert(length(unique(trainLabelRaw)) == length(unique(testLabelRaw)))
  
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
      filters = 100, kernel_size = 2, 
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
    # layer_average_pooling_1d() %>%
    # layer_flatten() %>%
    
    # Add a vanilla hidden layer:
    #layer_dense(100) %>%
    
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
  confusionMatrix <- t(matrix(table(factor(predictMax, 
                                         levels =  levels(testLabelRaw)),
                                  factor(testLabelRaw, 
                                         levels =  levels(testLabelRaw))), 
                            ncol = length(levels(testLabelRaw))))

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
  assert(all(accByClass <= 1))
  names(accByClass) <-  levels(trainLabelRaw)
  
  return(list(acc = evaluationResult$acc,
              loss = evaluationResult$loss,
              confusionMatrix = confusionMatrix,
              ProbAccDT = ProbAccDT,
              accByClass = accByClass,
              predictions = predictProb,
              testLabelRaw = testLabelRaw))
}



predictCNNSeq <- function(dataPath, fileName, 
                       upSampling = FALSE, epochs = 12) {
  assertString(dataPath)
  assertString(fileName)
  assertNumber(epochs)
  assertFlag(upSampling)
  
  print("read in Data")
  data <- readRDS(paste0(dataPath, fileName))
  assertList(data)
  assertSubset(names(data), choices = c("resultTrain", "resultTest",
                                        "labelTrain", "labelTest",
                                        "IndexDT", "maxWords", "channels"))
  
  trainData <- data$resultTrain
  if(upSampling) trainData <- generalizedSampling(data = trainData, 
                                                  method = "up", 
                                                  label = "labelRaw")
  testData <- data$resultTest
  trainLabelRaw <- data$labelTrain
  assertFactor(trainLabelRaw)
  testLabelRaw <- data$labelTest
  assertFactor(testLabelRaw)
  
  maxWords <- data$maxWords
  channels <- data$channels
  rm(data)  
  
  assert(length(unique(trainLabelRaw)) == length(unique(testLabelRaw)))
  
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
    layer_dropout(0.1) %>%
    
    # Add a Convolution1D, which will learn filters
    layer_conv_1d(filters = 100, kernel_size  = 2, 
      padding = "valid", activation = "relu", strides = 1,
      name = "conv1") %>%
    layer_dropout(0.1) %>%
    layer_conv_1d(filters = 100, kernel_size = 3,
                  padding = "same", activation = "relu",
                  strides = 1,
                  name = "conv2") %>%
    layer_dropout(0.1) %>%
    layer_conv_1d(filters = 100, kernel_size = 4,
                  padding = "same", activation = "relu",
                  strides = 1,
                  name = "conv3") %>%
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
    optimizer = optimizer_adam(lr = 0.001),
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
  confusionMatrix <- t(matrix(table(factor(predictMax, 
                                  levels =  levels(testLabelRaw)),
                           factor(testLabelRaw, 
                                  levels =  levels(testLabelRaw))),
                           ncol = length(levels(testLabelRaw))))
  
  confAcc <- sum(diag(confusionMatrix)) / sum(confusionMatrix)
  
  # checking accuracy of model and from confusion matrix
  print(paste("model accuracy is", evaluationResult$acc,
              "and confusion matrix accuracy is", confAcc))
  
  # naming rows and cols
  rownames(confusionMatrix) <- levels(testLabelRaw)
  colnames(confusionMatrix) <- levels(testLabelRaw)

    print(paste("sum of confusionMatrix is ", sum(confusionMatrix)))
        
  accByClass <- diag(as.matrix(confusionMatrix)) / colSums(testLabel)
  assert(all(accByClass <= 1))
  names(accByClass) <-  levels(trainLabelRaw)

  
  return(list(acc = evaluationResult$acc,
              loss = evaluationResult$loss,
              confusionMatrix = confusionMatrix,
              ProbAccDT = ProbAccDT,
              accByClass = accByClass,
              predictions = predictProb,
              testLabelRaw = testLabelRaw))
}


predictLSTMSeq <- function(dataPath, fileName, 
                        upSampling = FALSE, epochs = 12) {
  assertString(dataPath)
  assertString(fileName)
  assertNumber(epochs)
  assertFlag(upSampling)
  
  print("read in Data")
  data <- readRDS(paste0(dataPath, fileName))
  assertList(data)
  assertSubset(names(data), choices = c("resultTrain", "resultTest",
                                        "labelTrain", "labelTest",
                                        "IndexDT", "maxWords", "channels"))
  
  trainData <- data$resultTrain
  if(upSampling) trainData <- generalizedSampling(data = trainData, 
                                                  method = "up", 
                                                  label = "labelRaw")
  testData <- data$resultTest
  trainLabelRaw <- data$labelTrain
  assertFactor(trainLabelRaw)
  testLabelRaw <- data$labelTest
  assertFactor(testLabelRaw)
  
  maxWords <- data$maxWords
  channels <- data$channels
  rm(data)  
  
  assert(length(unique(trainLabelRaw)) == length(unique(testLabelRaw)))
  
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
    optimizer = optimizer_adam(lr = 0.001),
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
  confusionMatrix <- t(matrix(table(factor(predictMax, 
                                         levels =  levels(testLabelRaw)),
                                  factor(testLabelRaw, 
                                         levels =  levels(testLabelRaw))),
                            ncol = length(levels(testLabelRaw))))
  
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
  
  
  # accuracy by class: colSums are the amount of true labels
  accByClass <- diag(confusionMatrix) / colSums(confusionMatrix)
  assert(all(accByClass <= 1))
  names(accByClass) <-  levels(testLabelRaw)
  
  
  return(list(acc = evaluationResult$acc,
              loss = evaluationResult$loss,
              confusionMatrix = confusionMatrix,
              ProbAccDT = ProbAccDT,
              accByClass = accByClass,
              predictions = predictProb,
              testLabelRaw = testLabelRaw))
}

predictLSTMArray <- function(dataPath, fileName, 
                             upSampling = FALSE, epochs = 12) {
  assertString(dataPath)
  assertString(fileName)
  assertNumber(epochs)
  assertFlag(upSampling)
  
  print("read in Data")
  data <- readRDS(paste0(dataPath, fileName))
  assertList(data)
  assertSubset(names(data), choices = c("resultTrain", "resultTest",
                                        "labelTrain", "labelTest",
                                        "IndexDT", "maxWords", "channels"))
  
  trainData <- data$resultTrain
  if(upSampling) trainData <- generalizedSampling(data = trainData, 
                                                  method = "up", 
                                                  label = "labelRaw")
  testData <- data$resultTest
  trainLabelRaw <- data$labelTrain
  assertFactor(trainLabelRaw)
  testLabelRaw <- data$labelTest
  assertFactor(testLabelRaw)
  
  maxWords <- data$maxWords
  channels <- data$channels
  rm(data)  
  
  assert(length(unique(trainLabelRaw)) == length(unique(testLabelRaw)))
  
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
    # layer_conv_1d(input_shape  = list(maxWords, channels),
    #               data_format = "channels_last",
    #               filters = 50, kernel_size = 2, 
    #               padding = "same", activation = "relu", strides = 1,
    #               name = "conv1"#, trainable = FALSE
    # ) %>%
    # bidirectional(layer_lstm(units = 128)) %>%
    bidirectional(input_shape =  list(maxWords, channels),
                  layer_lstm(units = 256)) %>%
    layer_dropout(rate = 0.4) %>% 
    layer_activation("relu") %>%
    
    # # Add a vanilla hidden layer:
    # layer_dense(units = 100) %>%
    
    # Apply 20% layer dropout
    #layer_dropout(0.2) %>%
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
  confusionMatrix <- t(matrix(table(factor(predictMax, 
                                         levels =  levels(testLabelRaw)),
                                  factor(testLabelRaw, 
                                         levels =  levels(testLabelRaw))),
                            ncol = length(levels(testLabelRaw))))
 
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
  assert(all(accByClass <= 1))
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
  confusionMatrix <- t(matrix(table(factor(predictions, levels = levels(truth)),
                           factor(truth, levels = levels(truth))), 
                           ncol = length(levels(truth))))
  row.names(confusionMatrix) <- levels(truth)
  
 return(list(acc = accuracy,
             confusionMatrix = confusionMatrix))
}

