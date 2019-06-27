
# --------H2o--------------------------------
predictH2o <- function(dataPath = paste0("03_computedData/04_preparedData/",
                     "NewsWithLabel0.01Subset.fst"),
                     trainRatio = 0.75){
  
  h2o.init()
 
  h2o.init(nthreads=-1, max_mem_size="2G")
  h2o.removeAll()
  
  data <- read.fst(dataPath, as.data.table = TRUE)
  indexes <- sample.int(nrow(data), size = round(nrow(data) * trainRatio))
  trainData <- data[indexes]
  testData <- data[-indexes]
  trainData.h2o <- as.h2o(trainData)
  testData.h2o <- as.h2o(testData)
  model.h2o <- h2o.deeplearning(y = "labelRaw", training_frame = trainData.h2o, 
                                model_id = "1")
  
  predict.h2o <- h2o.predict(object = model.h2o, newdata = testData.h2o)
  predict.local <- as.data.table(predict.h2o)
  
  predictions <- as.character(predict.local[, predict][[1]])
  predict.local[, predict := NULL]

  confMat <- h2o.confusionMatrix(model.h2o)
  truth <- oneHotEncode(testData$labelRaw)
  
  resultDiffProb <- (truth - predict.local)^2    
  meanSquareError <- sum(resultDiffProb)/prod(dim(predict.local))
  # 0.03627311
  accuracy <- mean(as.character(testData$labelRaw) == predictions)
  
  return(list(meanSquareError = meanSquareError,
              accuracy = accuracy))
}


predictXgBoost <- function(dataPath = paste0("03_computedData/04_preparedData/",
                                         "NewsWithLabel0.01Subset.fst"),
                       trainRatio = 0.75){
  
  data <- read.fst(dataPath, as.data.table = TRUE)
  indexes <- sample.int(nrow(data), size = round(nrow(data) * trainRatio))
  trainData <- data[indexes]
  testData <- data[-indexes]
 
  trainLabel <- as.numeric(trainData$labelRaw) - 1
  testLabel <- as.numeric(testData$labelRaw) - 1
  trainData[, labelRaw := NULL]
  testData[, labelRaw := NULL]
  
  numClass <- length(unique(trainLabel))
  eval_metric <- "mlogloss"
  objective <- "multi:softprob"
  nrounds <- 20
  
  trainMatrix <- xgb.DMatrix(data = as.matrix(trainData), label = trainLabel)
  testMatrix <- xgb.DMatrix(as.matrix(testData), label = testLabel)
  
  model = xgboost::xgb.train(eval_metric = eval_metric,
                             objective = objective,
                             num_class = numClass,
                             data = trainMatrix, 
                             nrounds = nrounds, 
                             verbose = 2)
  predictions <- predict(model, newdata =  testMatrix, reshape = TRUE)
  predictionsMax <- sapply(data.table(t(predictions)), which.max) - 1
  
  truth <- oneHotEncode(testLabel)
  resultDiffProb <- (truth - predictions)^2    
  meanSquareError <- sum(resultDiffProb)/prod(dim(resultDiffProb))
  # 0.03627311
  accuracy <- mean(testLabel == predictionsMax)
  # 0.2968127
  
  
  # if (!is.null(testData)) {
  #   testMatrix = xgb.DMatrix(as.matrix(testData), label = testLabel)
  #   watchlist = list(dtest = testMatrix, dtrain = trainMatrix)
  # } else {
  #   watchlist = list(dtrain = trainMatrix)
  # }
  
 
  
  return(list(meanSquareError = meanSquareError,
              accuracy = accuracy))
}




# ----------neuralnet -----------------------------------------
# 
# data <- read.fst("03_computedData/04_preparedData/NewsWithLabel0.01Subset.fst",
#                  as.data.table = TRUE)
# newNames <- colnames(data)
# for (symbol in 100:1){
#   newNames <- gsub(newNames, pattern = symbol, replacement = paste0("V", symbol)
#                    , fixed = TRUE)
# }
# setnames(data, colnames(data), newNames)
# # try first neural net, nnet cant handle too many weights
# # try neuralnet now
# 
# target <- "labelRaw"
# dependentCols <- newNames[!(newNames %in% target)]
# 
# # trainData <- data[, .SD, .SDcols = dependentCols]
# # trainLabel <- data[[target]]
# 
# labelEncoded <- oneHotEncode(data$labelRaw)
# labelNames <- names(labelEncoded)
# newLabelNames <- gsub(labelNames, pattern = "&", replacement = "_")
# newLabelNames <- gsub(newLabelNames, pattern = " ", replacement = "")
# setnames(labelEncoded, labelNames, newLabelNames)
# 
# dataNnet <- data.table(labelEncoded, data[, .SD, .SDcols = dependentCols])
# 
# f <- as.formula(paste(paste(newLabelNames, collapse = "+"),
#                       " ~ ", 
#                       paste(dependentCols ,
#                             collapse= "+")))
# modelNnet <- neuralnet::neuralnet(formula = f,
#                                   data = dataNnet, hidden = c(100))
# # saving the model
# saveRDS(modelNnet, file = "03_computedData/05_modelData/modelNnet.rds")
# 
# resultResponse <- data.table(modelNnet$response)
# resultProbs <- data.table(modelNnet$net.result[[1]])
# 
# resultDiffProb <- abs(resultResponse - resultProbs)    
# errorProbDiff <- sum(resultDiffProb)/prod(dim(resultDiffProb))
# # 0.03408962 with training data only, hidden = c(3)
