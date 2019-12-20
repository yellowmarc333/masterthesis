#' @author Marc Schmieder
#' @description calculates linear regression model, fits predictions
#' @param trainData
#' @param trainLabel
#' @param valData
#' @param params  list of relevant parameters for this model
#' @param testData
#' @param testLabel
#' @return list of model data, model and predictions
predictLogisticRegression <- function(trainData, trainLabel, valData, params = list(),
                                      testData, testLabel) {

  print("calculating LiblineaR Model")
  model = LiblineaR(data = trainData, target = trainLabel, type = 6)
  # model = speedglm(trainLabel ~ KumVerzug, family = binomial(link='logit'), data = trainData)
  # corrplot
  # model = stats::glm(trainLabel ~ KumVerzug, family = binomial(link='logit'), 
  #                  data = trainData)
  
  valData = valData[, colnames(valData) %in% colnames(trainData)]
  
  if (nrow(valData) > 0) {
    predictions = predict(model, valData, proba = T)$probabilities[,"TRUE"]
  } else {
    predictions = c()
  }
  return(list(predictions = predictions,
              model = model,
              trainData = trainData,
              trainLabel = trainLabel,
              testData = testData,
              testLabel = testLabel))
}


#' @author Marc Schmieder
#' @description calculates decision tree model, fits predictions
#' @param trainData
#' @param trainLabel
#' @param valData
#' @param params  list of relevant parameters for this model
#' @param testData
#' @param testLabel
#' @return list of model data, model and predictions
predictDecisionTree <- function(trainData, trainLabel, valData, params = list(),
                                testData, testLabel){
  model = tree(Ausfallkennzeichen ~., data = cbind(trainData, 
                                                   data.frame(Ausfallkennzeichen=as.factor(trainLabel))))
  
  valData = valData[, colnames(valData) %in% colnames(trainData)]
  # adjust: if you use this model, change [,2] to something like [, "TRUE"] so there could be no errors.
  if (nrow(valData) > 0) {
    predictions = predict(model, valData)[, 2]
  } else {
    predictions = c()
  }
  
  return(list(predictions = predictions,
              model = model,
              trainData = trainData,
              trainLabel = trainLabel,
              testData = testData,
              testLabel = testLabel))
}


#' @author Felix Kleine Bösing
#' @description calculates linear regression
#' @param trainData
#' @param trainLabel
#' @param valData
#' @return 
predictRandomForest <- function(trainLabel, testLabel, trainData, testData, valData, params = list()) {
  model = ranger::ranger(dependent.variable.name = "Ausfallkennzeichen", 
                         data=cbind(trainData, data.frame(Ausfallkennzeichen=as.integer(trainLabel))),
                         importance = "impurity")
  valData = valData[, colnames(valData) %in% colnames(trainData)]
  if (nrow(valData) > 0) {
    predictions = predict(model, valData)$predictions
  } else {
    predictions = c()
  }
  return(list(predictions = predictions,
              model = model,
              trainData = trainData,
              trainLabel = trainLabel,
              testData = testData,
              testLabel = testLabel))
}


#' @author Felix Kleine Bösing
#' @description calculates linear regression
#' @param trainData
#' @param trainLabel
#' @param valData
#' @return Nothing. writes files as fst
predictOnlyZeros <- function(trainLabel, testLabel, trainData, testData, valData, params = list()) {
  return(list(predictions = rep(0, nrow(valData)),
              model = NULL,
              trainData = trainData,
              trainLabel = trainLabel,
              testData = testData,
              testLabel = testLabel))
}

#' @author Felix Kleine Bösing
#' @description calculates xgboost
#' @param trainData
#' @param trainLabel
#' @param valData
#' @return list containing predictions, model, trainData, trainLabel, testData and testLabel
predictXGBoost <- function(trainLabel, testLabel, trainData, testData, valData, params = list()) {
  if (!"max_depth" %in% names(params)) {
    print("will default max_depth to 5")
    params[["max_depth"]] = 5
  }
  if (!"eta" %in% names(params)) {
    print("will default eta to 0.3")
    params[["eta"]] = 0.3
  }
  if ("nrounds" %in% names(params)) {
    nrounds = params[["nrounds"]]
    params[["nrounds"]] = NULL
  } else {
    print("Will default nrounds to 100")
    nrounds = 50
  }
  params[["silent"]] = 0
  if ("objective" %in% names(params)) {
    objective = params[["objective"]]
  } else {
    print("Will default nrounds to binary:logistic")
    params[["objective"]] = "binary:logistic"
  }

  trainMatrix = xgb.DMatrix(as.matrix(trainData), label = trainLabel)
  
  if (!is.null(testData)) {
    testMatrix = xgb.DMatrix(as.matrix(testData), label = testLabel)
    watchlist = list(dtrain = trainMatrix, dtest = testMatrix)
  } else {
    watchlist = list(dtrain = trainMatrix)
  }
  model = xgboost::xgb.train(params, trainMatrix, 
                             nrounds = nrounds, 
                             verbose = 1, 
                             watchlist = watchlist)
  
  # keep only columns of validationData that is present in trainData since they otherwise have no predictive value and aren´t accounted in any trees
  #valData = valData[, colnames(valData) %in% colnames(trainData)]
  #colNamesVal = colnames(valData)
  # for (col in colnames(trainData)) {
  #   if (!col %in% colNamesVal) {
  #     valData[[col]] = 0
  #   }    
  # }

  if (!is.null(valData)) {
    predictions = predict(model, as.matrix(valData))
  } else {
    predictions = c()
  }
  return(list(predictions = predictions,
              model = model,
              trainData = trainData,
              trainLabel = trainLabel,
              testData = testData,
              testLabel = testLabel))
}

predictProphet <- function(trainLabel, testLabel, trainData, testData, 
                           valData, params = list(periods = 365)) {
  dt <- data.table(trainData, y = trainLabel)

  trainData <- dt[, .(dtGet("y"), dtGet("ds"))]
  setnames(trainData, c("y", "ds"))
  
  print("calculating prophet")
  model <- prophet::prophet(trainData, daily.seasonality = TRUE, 
                            yearly.seasonality = TRUE)
  future <- prophet::make_future_dataframe(model, periods = 365)

  forecast <- predict(model, future)
  
  if (!is.null(valData)) {
    predictions = forecast$yhat
  } else {
    predictions = c()
  }
  return(list(predictions = predictions,
              model = model,
              trainData = trainData,
              trainLabel = trainLabel,
              testData = testData,
              testLabel = testLabel))
}

