

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
  
  fileName <- fileName6

  data <- read.fst(paste0(dataPath, fileName), as.data.table = TRUE)
  
  indexes <- sample.int(nrow(data), size = round(nrow(data) * trainRatio))
  trainData <- data[indexes]
  trainData <- trainData - min(trainData)
  testData <- data[-indexes]
  testData <- testData - min(testData)
  
  trainLabel <- to_categorical(as.numeric(trainData$labelRaw) -1)
  testLabel <- to_categorical(as.numeric(testData$labelRaw) -1)
  trainData[, labelRaw := NULL]
  testData[, labelRaw := NULL]
  
  wordVectors <- read.fst("03_computedData/04_preparedData/WordVectors-0.1-50-FALSE.fst",
                          as.data.table = TRUE)
  
  
  input <- layer_input(
    shape = list(NULL),
    dtype = "int32",
    name = "input"
  )
  
  
  model <- keras_model_sequential()
  model %>% 
    layer_embedding(name = "embedding", input_dim = ncol(trainData)+1,
                output_dim = 32) %>% 
    layer_global_average_pooling_1d() %>%
    # layer_lstm(units = 50, dropout = 0.25, 
    #            recurrent_dropout = 0.25, 
    #            return_sequences = FALSE, name = "lstm") %>% 
    layer_dense(units = 32, activation = "sigmoid", name = "dense",
                input_shape = 1) %>% 
    layer_dense(units = ncol(trainLabel),
                activation = "softmax", 
                name = "predictions")
  
  summary(model)
  
  
  # Bring model together
  model <- keras_model(input, predictions)
  
  # Freeze the embedding weights initially to prevent updates propgating 
  # back through and ruining our embedding
  # get_layer(model, name = "embedding") %>% 
  #   set_weights(list(wordVectors)) %>% 
  #   freeze_weights()
  
  
  # Compile
  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(),
    metrics = c('accuracy')
  )
  
  # Print architecture (plot_model isn't implemented in the R package yet)

  history <- model %>% fit(
    as.matrix(trainData),
    trainLabel,
    batch_size = 2048,
    validation_data = list(as.matrix(testData), testLabel),
    epochs = 35,
    view_metrics = FALSE,
    verbose = 0
  )
  
  # Look at training results
  
  print(history)
  plot(history)
  
  
}

