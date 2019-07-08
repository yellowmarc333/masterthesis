

predictNN <- function(dataPath, fileName, trainRatio = 0.75){
  assertString(dataPath)
  assertString(fileName)
  assertNumber(trainRatio, lower = 0, upper = 1)
  
  h2o.init(nthreads=-1, max_mem_size="16G")
  h2o.removeAll()
  
  data <- read.fst(paste0(dataPath, fileName), as.data.table = TRUE)
  
  indexes <- sample.int(nrow(data), size = round(nrow(data) * trainRatio))
  trainData <- data[indexes]
  testData <- data[-indexes]
  testLabel <- as.character(testData$labelRaw)
  
  trainData.h2o <- as.h2o(trainData)
  testData.h2o <- as.h2o(testData)
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


predictXG <- function(dataPath, fileName, trainRatio = 0.75){
  assertString(dataPath)
  assertString(fileName)
  assertNumber(trainRatio, lower = 0, upper = 1)
  
  data <- read.fst(paste0(dataPath, fileName), as.data.table = TRUE)
  indexes <- sample.int(nrow(data), size = round(nrow(data) * trainRatio))
  trainData <- data[indexes]
  testData <- data[-indexes]

  trainLabel <- as.numeric(trainData$labelRaw) - 1
  testLabel <- as.numeric(testData$labelRaw) - 1
  trainData[, labelRaw := NULL]
  testData[, labelRaw := NULL]

  # create watchlist
  watchIndexes <- sample.int(nrow(trainData), 
                             size = round(nrow(trainData) * trainRatio))
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


predictRF <- function(dataPath, fileName, trainRatio = 0.75) {
  assertString(dataPath)
  assertString(fileName)
  assertNumber(trainRatio, lower = 0, upper = 1)
  
  data <- read.fst(paste0(dataPath, fileName), as.data.table = TRUE)
  
  indexes <- sample.int(nrow(data), size = round(nrow(data) * trainRatio))
  trainData <- data[indexes]
  testData <- data[-indexes]
  
  testLabel <- as.numeric(testData$labelRaw) - 1
  
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


predictKeras <- function(dataPath, fileName, trainRatio = 0.75) {
  assertString(dataPath)
  assertString(fileName)
  assertNumber(trainRatio, lower = 0, upper = 1)
  

  dataRDS <- readRDS(paste0(dataPath, fileName))
  data <- dataRDS[["wordVectorArray"]]
  label <- as.factor(dataRDS[["label"]])
  maxWords <- dataRDS[["maxWords"]]
  channels <- dataRDS[["channels"]]
  
  indexes <- sample.int(dim(data)[1], size = round(dim(data)[1] * trainRatio))
  trainData <- data[indexes, , ]
  testData <- data[-indexes, , ]
  
  trainLabel <- to_categorical(as.numeric(label[indexes]) - 1)
  testLabel <- to_categorical(as.numeric(label[-indexes]) - 1)
  

  model <- keras_model_sequential()
  
  model %>% 
    
    # Add a Convolution1D, which will learn filters
    # Word group filters of size filter_length:
    layer_conv_1d(input_shape  = list(maxWords, channels),
                  data_format = "channels_last",
      filters = 100, kernel_size = 5, 
      padding = "valid", activation = "relu", strides = 1
    ) %>%
    # Apply max pooling:
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

  summary(model)
  model$input_shape
  
  # Compile
  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(),
    metrics = c('accuracy')
  )
  
  # Print architecture (plot_model isn't implemented in the R package yet)

  
  history <- model %>% fit(
    x = trainData,
    y = trainLabel,
    validation_data = list(testData, testLabel),
    view_metrics = FALSE,
    verbose = 2)
  
  # Look at training results
  
  print(history)
  plot(history)
  
  
}

