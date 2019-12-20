
#' @author Felix Kleine Bösing
#' @description prediction pipeline
#' @param data data used for training and predicting
#' @param label list with label 
#' @param predictor functional implementation of various models and preprocessing functions
#' @param validationIndices list of k vectors that keep the indices of validation tests
#' @param sampling can be either up or downsampling
#' @return Nothing. writes files as fst
predictionFramework <- function(rawData, label, predictors, validationIndices, sampling = "up", .N) {
 
  
  
  predictions = list()
  for (name in names(predictors)) {
    ModelDataList = list()
    print(paste0("Predicting model: ", name))
    predictionFunction = predictors[[name]]$model$name
    params = predictors[[name]]$model$params
    preprocessingFunctions = predictors[[name]]$preprocessing
    prediction = rep(NA, nrow(rawData))
    trainTestSplit = predictors[[name]]$model$trainTestSplit
    print("Applying preprocessing functions!")
    if (length(preprocessingFunctions) > 0) {
      for (preprocessingFunction in preprocessingFunctions) {
        data = preprocessingFunction(rawData)
      }
    } else {
      data = rawData
    }
    i = 1
    namesValIndices = names(validationIndices)
    
    for (validationIndex in validationIndices) {
      print(paste0("Calculation k-fold: ", i, "/", length(validationIndices)))
      trainData = data[validationIndex$train ,]
      trainLabel = label[validationIndex$train]
      ## subsplit the train dataset into further train and test for xgboost
      if (trainTestSplit < 1) {
        split = sample.int(nrow(trainData), 
                           as.integer((1-trainTestSplit)*nrow(trainData)))
        testData = trainData[split, ]
        testLabel = trainLabel[split]
        trainData = trainData[!seq(1, nrow(trainData)) %in% split,]
        trainLabel = trainLabel[!seq(1, length(trainLabel)) %in% split]
      } else {
        testData = NULL
        testLabel = NULL
      }
      print("Sampling traindata!")
      if (sampling == "up") {
        dataLabel = upSamplingData(trainData, trainLabel)
      } 
      if (sampling == "down") {
        dataLabel = downSamplingData(trainData, trainLabel)
      }
      if (sampling == "balance") {
        dataLabel = balanceSamplingData(trainData, trainLabel, .N = .N)
      }
      if(sampling == "none"){
        dataLabel = list(data = trainData, label = trainLabel)
      }

      trainData = dataLabel$data
      trainLabel = dataLabel$label

      valData = data[validationIndex$val,]
      print("Running Prediction")
  
      ModelData = predictionFunction(trainData = trainData, 
                                     testData =  testData,
                                     trainLabel = trainLabel, 
                                     testLabel = testLabel,
                                     valData = valData,
                                     params = params)
      
      singlePredictions = ModelData$predictions

      # ugly quickfix
      prediction[validationIndex$val] = singlePredictions[1:
                                                            length(validationIndex$val)]
      
      ModelDataList[[namesValIndices[[i]]]] = ModelData
      i = i + 1
    }

    predictions[[name]] = list(predictions = list(predictions = prediction,
                                                  actuals = label,
                                                  data = data),
                               models = ModelDataList)
  }
  return(predictions)
}