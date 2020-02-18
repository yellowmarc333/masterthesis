explainXGBoost <- function(inPath = "03_computedData/05_modelData/finalModels/", 
                           modelName = "mod_BOW_XG_Full_2020-01-29.RDS",
                           embeddingPath = "03_computedData/04_preparedData/BOW-Full-TRUE-FALSE.rds"){
  assertString(inPath)
  assertString(modelName)
  assertString(embeddingPath)
  
  embeddingData <- readRDS(embeddingPath)
  newData <- embeddingData[["resultTest"]]
  rm(embeddingData)
  
  modelData <- readRDS(file = paste0(inPath, modelName))
  
  model <- modelData[["model"]]
  testLabelRaw <- modelData[["testLabelRaw"]]
  
  # readin testSubsets
  testSubsets <- readRDS("03_computedData/06_evaluatedData/testSubsets.RDS")
  indexesRaw <- testSubsets$allAgreed[, .(BOW_XG_rowVec)][[1]]
  indexes <- indexesRaw[indexesRaw != 0]
  
  newDataSub <- newData[indexes,]
  testLabelSub <- testLabelRaw[indexes]
  
  predictions <- as.data.table(predict(model, 
                                       newdata =  newDataSub,
                                       reshape = TRUE))
  names(predictions) <- levels(testLabelRaw)
  predictionsMax <- apply(predictions, 1 , function(x) {
    names(x[which.max(x)])
  })
  mean(predictionsMax == testLabelSub)
  
}





explainMLP <- function(modelPath = "03_computedData/05_modelData/OnlyModelSave/mod_GloveSums300_MLP.h5",
                       embeddingPath = "03_computedData/04_preparedData/GloveSums-Full-300-FALSE.rds"){
  assertString(modelPath)
  assertString(embeddingPath)
  
  # load embeddings
  embeddingData <- readRDS(embeddingPath)
  newData <- embeddingData[["resultTest"]]
  testLabelRaw <- embeddingData[["labelTest"]]
  testLabelNumeric <- as.numeric(testLabelRaw) - 1
  names(testLabelNumeric) <- testLabelRaw
  testLabel <- to_categorical(testLabelNumeric)
  rm(embeddingData)
  

  # load keras model
  model <- load_model_hdf5(modelPath)
    
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

explainCNN <- function(modelPath = "03_computedData/05_modelData/OnlyModelSave/mod_GloveArrayFull300_CNN.h5",
                       embeddingPath = "03_computedData/04_preparedData/GloveArray-Full-300-FALSE.rds"){
  assertString(modelPath)
  assertString(embeddingPath)
  
  # load embeddings
  embeddingData <- readRDS(embeddingPath)
  newData <- embeddingData[["resultTest"]]
  testLabelRaw <- embeddingData[["labelTest"]]
  testLabelNumeric <- as.numeric(testLabelRaw) - 1
  names(testLabelNumeric) <- testLabelRaw
  testLabel <- to_categorical(testLabelNumeric)
  rm(embeddingData)
  
  # load keras model
  model <- load_model_hdf5(modelPath)
  
  # readin testSubsets
  # names(testSubsets$res)
  testSubsets <- readRDS("03_computedData/06_evaluatedData/testSubsets.RDS")
  indexesRaw <- testSubsets$allAgreed[, .(GloveArray300_CNNArray_rowVec)][[1]]
  indexes <- indexesRaw[indexesRaw != 0]
  
  newDataSub <- newData[indexes, ,]
  testLabelSub <- testLabel[indexes,]
  testLabelRawSub <- testLabelRaw[indexes]
  
  evaluationResult <- model %>% 
    evaluate(newDataSub, testLabelSub, batch_size = 32)
  predictProb <- data.table(model %>% 
                              predict(newDataSub, 
                                      testLabelSub, batch_size = 32))
  names(predictProb) <- levels(testLabelRaw)
  # see for which class the prop is the highest
  predictMax <- t(apply(predictProb, 1, function(x) {
    return(names(x[which.max(x)]))
  }))
  
  mean(predictMax == testLabelRawSub)
  
}

explainLSTM <- function(modelPath = "03_computedData/05_modelData/OnlyModelSave/mod_GloveArrayFull300_LSTM.h5",
                       embeddingPath = "03_computedData/04_preparedData/GloveArray-Full-300-FALSE.rds"){
  assertString(modelPath)
  assertString(embeddingPath)
  browser()
  # load embeddings
  embeddingData <- readRDS(embeddingPath)
  newData <- embeddingData[["resultTest"]]
  testLabelRaw <- embeddingData[["labelTest"]]
  testLabelNumeric <- as.numeric(testLabelRaw) - 1
  names(testLabelNumeric) <- testLabelRaw
  testLabel <- to_categorical(testLabelNumeric)
  rm(embeddingData)
  
  # load keras model
  model <- load_model_hdf5(modelPath)
  
  # readin testSubsets
  # names(testSubsets$res)
  testSubsets <- readRDS("03_computedData/06_evaluatedData/testSubsets.RDS")
  indexesRaw <- testSubsets$allAgreed[, .(GloveArray300_LSTMArray_rowVec)][[1]]
  indexes <- indexesRaw[indexesRaw != 0]
  
  newDataSub <- newData[indexes, ,]
  testLabelSub <- testLabel[indexes,]
  testLabelRawSub <- testLabelRaw[indexes]
  
  evaluationResult <- model %>% 
    evaluate(newDataSub, testLabelSub, batch_size = 32)
  predictProb <- data.table(model %>% 
                              predict(newDataSub, 
                                      testLabelSub, batch_size = 32))
  names(predictProb) <- levels(testLabelRaw)
  # see for which class the prop is the highest
  predictMax <- t(apply(predictProb, 1, function(x) {
    return(names(x[which.max(x)]))
  }))
  
  mean(predictMax == testLabelRawSub)
  
}



identifyTestSubsets <- function(inPath = "03_computedData/05_modelData/finalModelsBackup/",
                               embeddingPath = "03_computedData/04_preparedData/") {
  # origTestLabel ist die Kategorie aus dem Ursprungsdatensatz
  # testLabeltmp ist das vom Modell
  # assignInd sind die Indexe, die testLabeltmp im origTestLabel bekommt
  assertString(inPath)
  allModels <- list.files(path = inPath)
  # subset to only .rds files
  allModels <- allModels[grepl(allModels, pattern = ".RDS", fixed = TRUE)]
  
  origIndexes <- readRDS("03_computedData/03_integratedData/indexesFull.rds")
  dataRaw <- read.fst("03_computedData/02_cleanedData/News.fst",
                      as.data.table = TRUE)
  origValdata <- dataRaw[-origIndexes]
  origTestLabel <- origValdata$category
  
  res <- data.table()
  
  for(j in 1:length((allModels))) {
    model <- readRDS(paste0(inPath, allModels[j]))
    # aus der predictModel return data
    testLabelTmp <- model$testLabelRaw
    tmpName <- paste0(strsplit(allModels[j],
                               split = "_")[[1]][2:3], collapse = "_")

    embName <- switch(tmpName,
                      BOW_XG = "BOW-Full-TRUE-FALSE.rds",
                      GloveArray300_CNNArray = "GloveArray-Full-300-FALSE.rds",
                      GloveArray300_LSTMArray = "GloveArray-Full-300-FALSE.rds",
                      GloveSums300_MLP = "GloveSums-Full-300-FALSE.rds")
    
    predictions <- model[["predictions"]]
    predictMax <- apply(predictions, 1, function(x) {
      return(names(x[which.max(x)]))
    })

    embedding <- readRDS(paste0(embeddingPath, embName))
    indexDT <- embedding$IndexDT
    rm(embedding)
    
    testSetInd <- indexDT[isTrainSet == FALSE,]$isFiltered
    assignInd <- which(testSetInd)
    
    fillVec <- numeric(length(testSetInd))
    rowVec <- numeric(length(testSetInd))
    fillVec[assignInd] <- predictMax
    # um die stichprobe später für jedes Modell/embedding ziehen zu können
    rowVec[assignInd] <- 1:length(testLabelTmp)
    
    res[, predictMax := fillVec]
    res[, rowVec := rowVec]
    setnames(res, "predictMax", tmpName)
    setnames(res, "rowVec", paste0(tmpName, "_rowVec"))
  }

  res[, TestLabel := origTestLabel]
  res[, count := {
    Sum1 <- BOW_XG == TestLabel
    Sum2 <- GloveArray300_CNNArray == TestLabel
    Sum3 <- GloveArray300_LSTMArray == TestLabel
    Sum4 <- GloveSums300_MLP == TestLabel
    
    count = Sum1 + Sum2 + Sum3 + Sum4
    .(count)
    }]
  
  allAgreed <- res[count == 4]
  noneCorrect <- res[count == 0]

  return(list(res = res,
              allAgreed = allAgreed,
              noneCorrect = noneCorrect))
}

# model <- load_model_hdf5("03_computedData/05_modelData/OnlyModelSave/mod_GloveArrayFull300_CNN.h5")
# model <- load_model_hdf5("03_computedData/05_modelData/OnlyModelSave/mod_GloveArrayFull300_LSTM.h5")



