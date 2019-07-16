

predictNN <- function(dataPath, fileName, indexName){
  assertString(dataPath)
  assertString(fileName)
  assertString(indexName)

  h2o.init(nthreads=-1, max_mem_size="16G")
  h2o.removeAll()
  data <- read.fst(paste0(dataPath, fileName), as.data.table = TRUE)

  indexes <- read.fst(paste0(dataPath, indexName), as.data.table = TRUE)[[1]] 
  
  trainData <- data[indexes]
  testData <- data[-indexes]
  testLabel <- as.character(testData$labelRaw)
  
  trainData.h2o <- as.h2o(trainData)
  testData.h2o <- as.h2o(testData)
  
  print("fitting model")
  model <- h2o.deeplearning(y = "labelRaw", training_frame = trainData.h2o, 
                                model_id = "1")
  
  # predict testData and evaluate metrics
  predict.h2o <- h2o.predict(object = model, newdata = testData.h2o)
  predict.local <- as.data.table(predict.h2o)
  
  predictions <- as.character(predict.local[, predict])
  predict.local[, predict := NULL]
  
  truth <- oneHotEncode(testData$labelRaw)
  resultDiffProb <- (truth - predict.local)^2 
  
  meanSquareError <- sum(resultDiffProb)/prod(dim(resultDiffProb))
  accuracy <- mean(testLabel == predictions)
  
  return(list(meanSquareError = meanSquareError,
              accuracy = accuracy,
              model = model))
}


predictXG <- function(dataPath, fileName, indexName, subsetSize){
  assertString(dataPath)
  assertString(fileName)
  assertString(indexName)

  browser()
  data <- read.fst(paste0(dataPath, fileName), as.data.table = TRUE)
  indexes <- read.fst(paste0(dataPath, indexName), as.data.table = TRUE)[[1]] 
  
  trainData <- data[indexes]
  testData <- data[-indexes]

  trainLabel <- as.numeric(trainData$labelRaw) - 1
  testLabel <- as.numeric(testData$labelRaw) - 1
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
  
  # add Parameters
  numClass <- length(unique(trainLabel))
  eval_metric <- "mlogloss"
  objective <- "multi:softprob"
  nrounds <- 20
  
  print("training xgboost model")
  model = xgboost::xgb.train(eval_metric = eval_metric,
                             objective = objective,
                             num_class = numClass,
                             data = watchTrainMat, 
                             nrounds = nrounds, 
                             verbose = 1, 
                             watchlist = watchlist)

  # evaluating predictions and eval metrics
  predictions <- predict(model, newdata =  testMat, reshape = TRUE)
  predictionsMax <- sapply(data.table(t(predictions)), which.max) - 1
  
  truth <- oneHotEncode(testLabel)
  resultDiffProb <- (truth - predictions)^2    
  
  meanSquareError <- sum(resultDiffProb)/prod(dim(resultDiffProb))
  accuracy <- mean(testLabel == predictionsMax)

  return(list(meanSquareError = meanSquareError,
              accuracy = accuracy,
              model = model))
}


predictRF <- function(dataPath, fileName, indexName, subsetSize) {
  assertString(dataPath)
  assertString(fileName)
  assertString(indexName)

  data <- read.fst(paste0(dataPath, fileName), as.data.table = TRUE)
  
  indexes <- read.fst(paste0(dataPath, indexName), as.data.table = TRUE)[[1]]  
  
  trainData <- data[indexes]
  testData <- data[-indexes]
  
  testLabel <- as.numeric(testData$labelRaw) - 1
  
  print("fitting model")
  model = ranger::ranger(dependent.variable.name = "labelRaw", 
                         data = trainData,
                         importance = "impurity",
                         probability = TRUE)

  predictions = predict(model, testData, type = "response")$predictions
  
  predictionsMax <- sapply(data.table(t(predictions)), which.max) - 1
  
  truth <- oneHotEncode(testData$labelRaw)
  resultDiffProb <- (truth - predictions)^2    
  
  meanSquareError <- sum(resultDiffProb)/prod(dim(resultDiffProb))
  accuracy <- mean(testLabel == predictionsMax)
  
  return(list(meanSquareError = meanSquareError,
              accuracy = accuracy,
              model = model))
}


predictCNN <- function(dataPath, fileName, indexName, subsetSize) {
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
  
  
  trainLabel <- to_categorical(as.numeric(label[indexes]) - 1)
  testLabel <- to_categorical(as.numeric(label[-indexes]) - 1)
  
  # building model with layers
  model <- keras_model_sequential()
  model %>% 
    # Add a Convolution1D, which will learn filters
    # Word group filters of size filter_length:
    layer_conv_1d(input_shape  = list(maxWords, channels),
                  data_format = "channels_last",
      filters = 50, kernel_size = 2, 
      padding = "same", activation = "relu", strides = 1,
      name = "conv1"
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
    optimizer = optimizer_rmsprop(lr = 0.001),
    metrics = c('accuracy')
  )

  print("fitting model")
  
  history <- model %>% fit(
    x = trainData,
    y = trainLabel,
    epochs = 4,
    batchsize = 32,
    validation_data = list(testData, testLabel),
    view_metrics = FALSE,
    verbose = 2)
  
  # Look at training results
  # summary(model)
  # print(history)
  # plot(history)
  
  predictionResult <- model %>% evaluate(testData, testLabel, batch_size = 32)
  
  return(predictionResult)
}


predictEmb <- function(dataPath, fileName, indexName,  subsetSize) {
  assertString(dataPath)
  assertString(fileName)
  assertString(indexName)
  
  print("read in Data")
  data <- read.fst(paste0(dataPath, fileName), as.data.table = TRUE)
  label <- data$labelRaw

  indexes <- read.fst(paste0(dataPath, indexName), as.data.table = TRUE)[[1]]  
  
  trainData <- data[indexes, , ]
  testData <- data[-indexes, , ]
  
  trainData[, labelRaw := NULL]
  testData[, labelRaw := NULL]
  
  trainLabelRaw <- label[indexes]
  testLabelRaw <- label[-indexes]
  
  print(paste("in train are number of uniques:", 
              length(unique(trainLabelRaw))))
  print(paste("in test are number of uniques:", 
              length(unique(testLabelRaw))))
  
  trainLabel <- to_categorical(as.numeric(label[indexes]) - 1)
  testLabel <- to_categorical(as.numeric(label[-indexes]) - 1)
  
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
    epochs = 15,
    batchsize = 32,
    validation_data = list(as.matrix(testData), testLabel),
    view_metrics = FALSE,
    verbose = 2)
  
  predictionResult <- model %>% 
    evaluate(as.matrix(testData), testLabel, batch_size = 32)

  return(predictionResult)
}




