#' @author Felix Kleine Bösing, refactored by Marc Schmieder
#' @description/disclaimer 
#' This function expects rds input files with content:
#' train, trainLabel, test, labelName and type ("regression" or "timeseries")
#' Applies modelling on the prepared dataset: search for 'adjust' project relevant modifications
#'              modelData calls 12_predictionFramework, 11_predictionHelper and 10_predictionModels.
#'              the output structure of the RDS should remain because deployment is depentend on the list structure of the RDS.
#' @param dataPath directory in which the data is located
#' @param targetPath directory for saving modelled data
#' @param mode ignore, not really used
#' @param sampling ignore, not really used
#' @param .NRatio ignore, not really used
#' @param numberOfFolds ignore, not really used
#' @param dataSets ignore, not really used
#' @param trainTestSplit default 0.8
#' @return Nothing. writes files as rds
modelData <- function(dataPath = "03_computedData/04_preparedData",
                      targetPath = "03_computedData/05_modelData/",
                      mode = "onlyTrain",
                      sampling = "none",
                      .NRatio = 2, 
                      numberOfFolds = 5,
                      dataSets = list("all"),
                      trainTestSplit = 0.8) {
  assertString(dataPath)
  assertString(targetPath)
  assertString(mode)
  assertNumber(numberOfFolds)

  allFiles = list.files(dataPath)
  dataSets = list()
  
  # create list with all files in dataPath 
  dataSets <- sapply(allFiles, function(x) {
    filename <- x
    dataSetName <- gsub(x, pattern = ".rds", replacement = "", fixed = TRUE)
    return(list(filename = filename,
                dataSetName = dataSetName))
  }, simplify = FALSE)

  for (dataSet in dataSets) {
    # Modelling Data ----
    # read in the data table on which the modelling is computed
    data <- readRDS(paste0(dataPath, dataSet$filename))
    assertSubset(choices = names(data), c("train", "trainLabel", "type",
                                          "labelName"))
    modelData = data[["train"]]
    type <- data[["type"]]
    labelName <- data[["labelName"]]
    assert(!is.null(type))
    assertDataTable(modelData)
    Label = data[["trainLabel"]]
    assertAtomic(Label, len = nrow(modelData))

    if (type == "regression") {
      # predictors list that hold the predictionFunction and several preprocessing functions for missing value handling, one hot encoding and normalizing
      # In params you are able to deliver all parameters that are used in the specified model
      # adjust: if you want to add new models, add them as a function in 10_predictionModels and call them here.
      # all outcommented Models will run.
      predictors <- list(
        xgBoost = list(model = list(
          name = predictXGBoost,
          params = list(nrounds = 30,
                        max_depth = 3,
                        eval_metric = "rmse",
                        objective = "reg:squarederror"),
          trainTestSplit = 0.7),
          preprocessing = NULL)
      )
    }
    if (type == "timeseries") {
      # predictors list that hold the predictionFunction and several preprocessing functions for missing value handling, one hot encoding and normalizing
      # In params you are able to deliver all parameters that are used in the specified model
      # adjust: if you want to add new models, add them as a function in 10_predictionModels and call them here.
      # all outcommented Models will run.
      predictors <- list(
        prophet = list(model = list(
          name = predictProphet,
          trainTestSplit = 0.7),
          preprocessing = NULL)
      )
    }
   
  
    # depending on choosen mode, assert the indices of train and test data.
    # adjust, if you want to implement another mode (not a simple fold), then add the sampling of the indices here
    valIndicesData = getIndicesForOnePrediction(Label, size = trainTestSplit)
    .NData = .NRatio * nrow(modelData)

    print("Train and predict Data")
    predictionsData = predictionFramework(modelData, Label, predictors, 
                                          valIndicesData, sampling = sampling,
                                         .N = .NData)
    
    
    for(i in names(predictors)) {
      predictionsData[[i]][["type"]] <- type
      predictionsData[[i]][["labelName"]] <- labelName
    }

    # the model data is saved. 
    saveRDS(predictionsData, paste0(targetPath, "model_",
                                    dataSet$dataSetName,".rds"))
    
    print("Modelling is done!")
  }
}

