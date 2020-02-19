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
  indexesRaw <- testSubsets$allCorrect[, .(BOW_XG_rowVec)][[1]]
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
  # test2 <- testSubsets$allCorrect
  # test3 <- test2[GloveArray300_CNNArray_rowVec != 0, GloveArray300_CNNArray]
  # fehlersuche: labels sind identisch muss in identifyTestsubset der fehler liegen
  indexesRaw <- testSubsets$allCorrect[, .(GloveArray300_CNNArray_rowVec)][[1]]
  indexes <- indexesRaw[indexesRaw != 0]
  
  newDataSub <- newData[indexes, ,]
  testLabelSub <- testLabel[indexes,]
  testLabelRawSub <- testLabelRaw[indexes]
  
  resPerm <- modifyEmbPerm(newData = newDataSub,
                       model = model,
                       testLabel = testLabelSub,
                       testLabelRaw = testLabelRawSub, permN = 30)
  

  resRem1 <- modifyEmbRem(newData = newDataSub,
                         model = model,
                         testLabel = testLabelSub,
                         testLabelRaw = testLabelRawSub,
                         nWords = 1)
  
  resRem2 <- modifyEmbRem(newData = newDataSub,
                          model = model,
                          testLabel = testLabelSub,
                          testLabelRaw = testLabelRawSub,
                          nWords = 2, nRemove = 30)
   
  resRem3 <- modifyEmbRem(newData = newDataSub,
                          model = model,
                          testLabel = testLabelSub,
                          testLabelRaw = testLabelRawSub,
                          nWords = 3, nRemove = 30)

  resRem4 <- modifyEmbRem(newData = newDataSub,
                          model = model,
                          testLabel = testLabelSub,
                          testLabelRaw = testLabelRawSub,
                          nWords = 4, nRemove = 30)

  resSeqForward <- modifyEmbSeq(newData = newDataSub, 
                          model = model,
                          testLabel = testLabelSub,
                          testLabelRaw = testLabelRawSub, 
                          forward = TRUE)
  
  return(list(resPerm = resPerm,
              resRem1 = resRem1,
              resRem2 = resRem2,
              resRem3 = resRem3,
              resRem4 = resRem4,
              resSeqForward = resSeqForward))
  
}

explainLSTM <- function(modelPath = "03_computedData/05_modelData/OnlyModelSave/mod_GloveArrayFull300_LSTM.h5",
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
  indexesRaw <- testSubsets$allCorrect[, .(GloveArray300_LSTMArray_rowVec)][[1]]
  indexes <- indexesRaw[indexesRaw != 0]
  
  newDataSub <- newData[indexes, ,]
  testLabelSub <- testLabel[indexes,]
  testLabelRawSub <- testLabelRaw[indexes]
  
  resPerm <- modifyEmbPerm(newData = newDataSub,
                           model = model,
                           testLabel = testLabelSub,
                           testLabelRaw = testLabelRawSub, permN = 30)
  
  
  resRem1 <- modifyEmbRem(newData = newDataSub,
                          model = model,
                          testLabel = testLabelSub,
                          testLabelRaw = testLabelRawSub,
                          nWords = 1)
  
  resRem2 <- modifyEmbRem(newData = newDataSub,
                          model = model,
                          testLabel = testLabelSub,
                          testLabelRaw = testLabelRawSub,
                          nWords = 2, nRemove = 30)
  
  resRem3 <- modifyEmbRem(newData = newDataSub,
                          model = model,
                          testLabel = testLabelSub,
                          testLabelRaw = testLabelRawSub,
                          nWords = 3, nRemove = 30)
  
  resRem4 <- modifyEmbRem(newData = newDataSub,
                          model = model,
                          testLabel = testLabelSub,
                          testLabelRaw = testLabelRawSub,
                          nWords = 4, nRemove = 30)
  
  resSeqForward <- modifyEmbSeq(newData = newDataSub, 
                                model = model,
                                testLabel = testLabelSub,
                                testLabelRaw = testLabelRawSub, 
                                forward = TRUE)
  
  return(list(resPerm = resPerm,
              resRem1 = resRem1,
              resRem2 = resRem2,
              resRem3 = resRem3,
              resRem4 = resRem4,
              resSeqForward = resSeqForward))
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
 
  NonUnique <- apply(res[, .(BOW_XG, GloveArray300_CNNArray,
                             GloveArray300_LSTMArray, GloveSums300_MLP)],
                     1, function(x) length(unique(x)))
  
  res[, NonUnique := NonUnique]
  allCorrect <- res[count == 4]
  noneCorrect <- res[count == 0]
  
  noneAgreed <- res[NonUnique == 4]

  return(list(res = res,
              allCorrect = allCorrect,
              noneCorrect = noneCorrect,
              noneAgreed = noneAgreed))
}

# model <- load_model_hdf5("03_computedData/05_modelData/OnlyModelSave/mod_GloveArrayFull300_CNN.h5")
# model <- load_model_hdf5("03_computedData/05_modelData/OnlyModelSave/mod_GloveArrayFull300_LSTM.h5")


modifyEmbPerm <- function(newData, model,
                          testLabel, testLabelRaw,
                          permN = 10){
  
  set.seed(123)
  permMat <- t(sapply(1:permN, function(x) {
    sample.int(dim(newData)[2], dim(newData)[2])
  }, simplify = TRUE))
  
  accuracy <- numeric(permN)
  for(i in seq_len(permN)){
    permInd <- permMat[i, ]
    newDataModif <- newData[, permInd, ]
    
    predictProb <- data.table(model %>% 
                                predict(newDataModif, 
                                        testLabel, batch_size = 32))
    names(predictProb) <- levels(testLabelRaw)
    # see for which class the prop is the highest
    predictMax <- t(apply(predictProb, 1, function(x) {
      return(names(x[which.max(x)]))
    }))
    
    accuracy[i] <- mean(predictMax == testLabelRaw)
  }
  
  res <- mean(accuracy)
  return(list(mean_accuracy = res,
              accuracy = accuracy))
}



modifyEmbRem <- function(newData, model,
                          testLabel, testLabelRaw,
                          nWords = 1, nRemove = 40){
  dims <- dim(newData)
  
  if(nWords == 1) {
    nRemove <- dims[2]
    removeIndexes <- matrix(1:dims[2], ncol = nWords, byrow = TRUE)
  }

  if(nWords > 1) {
    set.seed(123)
    removeIndexes <- t(sapply(1:nRemove, function(x) {
      sample.int(dims[2], nWords)
    }, simplify = TRUE))
  }
  
                          
  accuracy <- numeric(nrow(removeIndexes))
  
  for(i in seq_len(nrow(removeIndexes))){
    removeInd <- removeIndexes[i, ]
    newDataModif <- newData
    # an stelle removeInd alles auf 0 setzen
    newDataModif[, removeInd, ] <- rep(0, dims[3])
    
    predictProb <- data.table(model %>% 
                                predict(newDataModif, 
                                        testLabel, batch_size = 32))
    names(predictProb) <- levels(testLabelRaw)
    # see for which class the prop is the highest
    predictMax <- t(apply(predictProb, 1, function(x) {
      return(names(x[which.max(x)]))
    }))
    
    accuracy[i] <- mean(predictMax == testLabelRaw)
  }
  
  res <- mean(accuracy)
  return(list(mean_accuracy = res,
              accuracy = accuracy))
}


modifyEmbSeq <- function(newData, model,
                          testLabel, testLabelRaw,
                          forward = TRUE){
  dims <- dim(newData)
  # alle außer der erste werden 0 gesetzt.
  seqIndizes <- 2:(dims[2])
  
  accuracy <- numeric(length(seqIndizes))
  for(i in seqIndizes){
    permInd <- i:dims[2]
    newDataModif <- newData
    newDataModif[, permInd, ] <- rep(0, dims[3])
    
    predictProb <- data.table(model %>% 
                                predict(newDataModif, 
                                        testLabel, batch_size = 32))
    names(predictProb) <- levels(testLabelRaw)
    # see for which class the prop is the highest
    predictMax <- t(apply(predictProb, 1, function(x) {
      return(names(x[which.max(x)]))
    }))
    
    accuracy[i] <- mean(predictMax == testLabelRaw)
  }
  
  res <- mean(accuracy)
  return(list(mean_accuracy = res,
              accuracy = accuracy))
}



