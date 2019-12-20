#' @author Marc Schmieder
#' @description calculates decision tree model, fits predictions
#' @param trainData
#' @param trainLabel
#' @param valData
#' @param params  list of relevant parameters for this model
#' @param testData
#' @param testLabel
#' @return list of model data, model and predictions
simpleModelTree <- function(trainData, trainLabel, labelName = "EigentumsWert",
                            testData, testLabel, params = list()){

  dt <- data.table(trainData,  Label = trainLabel)
  setnames(dt, "Label", labelName)
  
  # hardcode
  model = tree(EigentumsWert ~., data = dt)
  
  # adjust: if you use this model, change [,2] to something like [, "TRUE"] so there could be no errors.
  if (nrow(testData) > 0) {
    predictions <- predict(model, testData)
    # rmse measure
    rmse <- sqrt(mean((predictions - testLabel)^2))
  } else {
    predictions = c()
  }
  
  return(list(predictions = predictions,
              model = model,
              rmse = rmse,
              trainData = trainData,
              trainLabel = trainLabel,
              testData = testData,
              testLabel = testLabel))
}