explainXGBoost <- function(inPath = "03_computedData/05_modelData/finalModels/", 
                           modelName = "mod_BOW_XG_Full_2020-01-29.RDS",
                           embeddingPath = "03_computedData/04_preparedData/BOW-Full-TRUE-FALSE.rds"){
  assertString(inPath)
  assertString(modelName)
  assertDataTable(embeddingPath)
  
  embeddingData <- readRDS(embeddingPath)
  newData <- embeddingData[["resultTest"]]
  rm(embeddingData)
  
  modelData <- readRDS(file = paste0(inPath, modelName))
  
  model <- modelData[["model"]]
  testLabelRaw <- modelData[["testLabelRaw"]]
  
  predictions <- as.data.table(predict(model, 
                                       newdata =  newData,
                                       reshape = TRUE))
  names(predictions) <- levels(testLabelRaw)
  predictionsMax <- apply(predictions, 1 , function(x) {
    names(x[which.max(x)])
  })
  
  
}





explainMLP <- function(inPath = "03_computedData/05_modelData/finalModels/", 
                       modelName = "mod_GloveSums300_MLP_Full_2020-01-30.RDS",
                       embeddingPath = "03_computedData/04_preparedData/GloveSums-10pc-300-FALSE.rds"){
  assertString(inPath)
  assertString(modelName)
  assertDataTable(embeddingPath)
  
  embeddingData <- readRDS(embeddingPath)
  newData <- embeddingData[["resultTest"]]
  rm(embeddingData)
  
  modelData <- readRDS(file = paste0(inPath, modelName))
  
  model <- load_model_hdf5("03_computedData/05_modelData/OnlyModelSave/mod_GloveSums300_MLP.h5")
  model <- load_model_hdf5("03_computedData/05_modelData/OnlyModelSave/mod_GloveArrayFull300_CNN.h5")
  model <- load_model_hdf5("03_computedData/05_modelData/OnlyModelSave/mod_GloveArrayFull300_LSTM.h5")
  
  
  
  
  
  testLabelRaw <- modelData[["testLabelRaw"]]
  # setting up testLabel
  testLabelNumeric <- as.numeric(testLabelRaw) - 1
  names(testLabelNumeric) <- testLabelRaw
  testLabel <- to_categorical(testLabelNumeric)
  
  evaluationResult <- model %>% 
    evaluate(as.matrix(newData), testLabel, batch_size = 32)
  
  predictProb <- data.table(model %>% 
                              predict(as.matrix(newData), 
                                      testLabel, batch_size = 32))
  names(predictProb) <- levels(trainLabelRaw)
  # see for which class the prop is the highest
  predictMax <- t(apply(predictProb, 1, function(x) {
    return(names(x[which.max(x)]))
  }))
  
  
  
}





