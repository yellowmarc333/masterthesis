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
  res[, TestLabelRawIndex := 1:.N]
 
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




explainIndividual <- function(modelPath = "03_computedData/05_modelData/OnlyModelSave/mod_GloveArrayFull300_CNN.h5",
                              embeddingPath = "03_computedData/04_preparedData/"){
  assertString(modelPath)
  assertString(embeddingPath)
  
  # readin testSubsets
  testSubsets <- readRDS(
    "03_computedData/06_evaluatedData/testSubsets.RDS")$noneAgreed

  # select all data points where lstm is correct and all models have different
  # prognosis
  candidates <- testSubsets[GloveArray300_LSTMArray == TestLabel]
  selectIndexes <- 1:nrow(candidates)
  indexData <- readRDS("03_computedData/03_integratedData/indexesFull.rds")
  
  dataRaw <- read.fst("03_computedData/02_cleanedData/News.fst", 
                      as.data.table = TRUE)
  testDataRaw <- dataRaw[-indexData]
  headlines <- testDataRaw[candidates$TestLabelRawIndex[selectIndexes]]$headline
  trueLabelCheck <- testDataRaw[candidates$TestLabelRawIndex[selectIndexes]]$category
  
  
  allEmbeddings <- list.files(embeddingPath)
  selEmbeddings <- c("GloveArray-Full-300-FALSE.rds", 
                     "GloveSums-Full-300-FALSE.rds",
                     "BOW-Full-TRUE-FALSE.rds")
  assertSubset(selEmbeddings, allEmbeddings)
  
  EmbIndexPairs <- list(list(embName = "BOW-Full-TRUE-FALSE.rds",
                             embIndexes = candidates$BOW_XG_rowVec[selectIndexes],
                             type = 1,
                             modelPath = "03_computedData/05_modelData/OnlyModelSave/xgb.RDS"),
                        list(embName = "GloveArray-Full-300-FALSE.rds",
                             embIndexes = candidates$GloveArray300_CNNArray_rowVec[selectIndexes],
                             type = 2,
                             modelPath = "03_computedData/05_modelData/OnlyModelSave/mod_GloveArrayFull300_CNN.h5"),
                        list(embName = "GloveArray-Full-300-FALSE.rds",
                             embIndexes = candidates$GloveArray300_LSTMArray_rowVec[selectIndexes],
                             type = 3,
                             modelPath = "03_computedData/05_modelData/OnlyModelSave/mod_GloveArrayFull300_LSTM.h5"),
                        list(embName = "GloveSums-Full-300-FALSE.rds",
                             embIndexes = candidates$GloveSums300_MLP_rowVec[selectIndexes],
                             type = 4,
                             modelPath = "03_computedData/05_modelData/OnlyModelSave/mod_GloveSumsFull300_MLP.h5"))
  
  res <- list()
  
  for(j in seq_along(EmbIndexPairs)) {
    # load embeddings
    embNameTmp <- EmbIndexPairs[[j]]$embName
    embeddingData <- readRDS(paste0(embeddingPath, embNameTmp))
    resultTest <- embeddingData[["resultTest"]]
    testLabelRaw <- embeddingData[["labelTest"]]
    embIndexesTmp <- EmbIndexPairs[[j]]$embIndexes
    typeTmp <- EmbIndexPairs[[j]]$type
    modelPathTmp <- EmbIndexPairs[[j]]$modelPath
    
    if(typeTmp %in% c(1, 4)) {
      embSubset <-resultTest[embIndexesTmp,]
    }
    if(typeTmp %in% c(2,3)) {
      embSubset <-resultTest[embIndexesTmp, , ]
    }

    
    if(typeTmp == 1) {
      rm(embeddingData)
      model <- readRDS(modelPathTmp)
      
      predictProb <- as.data.table(predict(model, 
                                           newdata =  embSubset,
                                           reshape = TRUE))
      names(predictProb) <- levels(testLabelRaw)
    }
    if(typeTmp %in% c(2, 3)) {
      testLabelNumeric <- as.numeric(testLabelRaw) - 1
      names(testLabelNumeric) <- testLabelRaw
      testLabel <- to_categorical(testLabelNumeric)[embIndexesTmp,]
      rm(embeddingData)
      
      model <- load_model_hdf5(modelPathTmp)
      predictProb <- data.table(model %>% 
                                  predict(embSubset, 
                                          testLabel, batch_size = 32))
      names(predictProb) <- levels(testLabelRaw)
    }
    
    
    if(typeTmp == 4) {
      testLabelNumeric <- as.numeric(testLabelRaw) - 1
      names(testLabelNumeric) <- testLabelRaw
      testLabel <- to_categorical(testLabelNumeric)[embIndexesTmp,]
      rm(embeddingData)
      
      model <- load_model_hdf5(modelPathTmp)
      predictProb <- data.table(model %>% 
                                  predict(as.matrix(embSubset), 
                                          testLabel, batch_size = 32))
      names(predictProb) <- levels(testLabelRaw)
    }
    
    predictMax <- t(apply(predictProb, 1, function(x) {
      return(names(x[which.max(x)]))
    }))[1,]
    
    res[[j]] <- list(embName = embNameTmp,
                     embIndexes = embIndexesTmp,
                     type = typeTmp,
                     modelPath = modelPathTmp,
                     predictProb = predictProb,
                     predictMax = predictMax,
                     trueLabelCheck = trueLabelCheck,
                     headlines = headlines)
  }
  return(res)
}


plotIndividualProbs <- function(explainData, index = 1) {
  assertList(explainData)
  assertNumber(index)

  
  res <- data.table()
  for(i in seq_along(explainData)) {
    resTmp <- explainData[[i]]
    
    
    predictProb <- as.numeric(resTmp$predictProb[index])
    
    res[, predictProb := predictProb]
    setnames(res, "predictProb", as.character(resTmp$type))
    
  }
  
  res[, Kategorie := names(resTmp$predictProb)]
  
  plotData <- melt(res, id.vars = "Kategorie")
  ggObj <- ggplot(plotData, aes(x = Kategorie,
                                y = value, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge") + 
    labs(x = "Nachrichtenkategorie",
         y = "Modellwahrscheinlichkeit",
         fill = "Embedding, Modell: ") +
    scale_fill_discrete(name = "Embedding, Modell", 
                         labels = c("BOW, XGBoost","GloVe 300D, CNN" ,
                                    "GloVe 300D, Bi-LSTM", 
                                    "SOW GloVe 300D, MLP")) +
    geom_text(aes(label = round(value, 3)), 
              position = position_dodge(width = 0.9),
              vjust = 0.5, hjust = 1.2,
              angle = 90, size = 2.4) +
    theme(axis.text.x  = element_text(angle = 45,
                                      vjust = 1, hjust = 1,
                                      size = 14),
          axis.text.y = element_text(size = 15),
          axis.title = element_text(size = 28),
          axis.ticks.x = element_line(size  = 1),
          legend.background = element_rect(fill = "lightgrey"),
          legend.key = element_rect(fill = "lightblue", color = NA),
          legend.position = "top",
          legend.text = element_text(size = 15),
          legend.title = element_text(size = 18),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) 
  
  return(ggObj)
}


plotLSTMSeq <- function(explainData, index = 1) {
  assertList(explainData)
  assertNumber(index)
  
  explainData <- res[[3]]
  embIndex <- explainData$embIndexes[index]
  
  modelPath = "03_computedData/05_modelData/OnlyModelSave/mod_GloveArrayFull300_LSTM.h5"
  embeddingPath = "03_computedData/04_preparedData/GloveArray-Full-300-FALSE.rds"

  # load embeddings
  embeddingData <- readRDS(embeddingPath)
  newDataWhole <- embeddingData[["resultTest"]]
  newDataSingle <- newDataWhole[embIndex, , ]
  newData <- array(newDataSingle, dim = c(1, dims[2], dims[3]))
  
  testLabelRaw <- embeddingData[["labelTest"]]
  testLabelNumeric <- as.numeric(testLabelRaw) - 1
  names(testLabelNumeric) <- testLabelRaw
  testLabel <- to_categorical(testLabelNumeric)[embIndex, ]
  rm(embeddingData)
  
  # load keras model
  model <- load_model_hdf5(modelPath)
  
  dims <- dim(newDataWhole)
  # alle außer der erste werden 0 gesetzt.
  seqIndizes <- 2:(dims[2])
  
  res <- matrix(numeric(length(seqIndizes)* 
                                  length(unique(testLabelRaw))),
                        ncol = length(unique(testLabelRaw)))
  for(i in seqIndizes){
    permInd <- i:dims[2]
    newDataModif <- newData
    newDataModif[, permInd, ] <- rep(0, dims[3])
    
    predictProb <- data.table(model %>% 
                                predict(newDataModif, 
                                        testLabel, batch_size = 32))
    
    # see for which class the prop is the highest
    res[i-1, ] <- as.numeric(predictProb)
  }
  
  resDT <- as.data.table(res)
  setnames(resDT, levels(testLabelRaw))
  resDT[, PartOfSequence := 1:.N]

  plotData <- melt(resDT, id.vars = "PartOfSequence")
  
  ggObj <- ggplot(plotData, aes(x = PartOfSequence,
                                y = value, color = variable)) +
    geom_line() +
    geom_point() +
    labs(x = "Wörter der Sequenz",
         y = "Modellwahrscheinlichkeit",
         color = "Kategorie: ") +
    guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
    theme(axis.text.x  = element_text(size = 14),
          axis.text.y = element_text(size = 15),
          axis.title = element_text(size = 28),
          axis.ticks.x = element_line(size = 1),
          legend.background = element_rect(fill = "lightgrey"),
          legend.key = element_rect(fill = "lightblue", color = NA),
          legend.position = "top",
          legend.text = element_text(size = 15),
          legend.title = element_text(size = 18))
  
  return(ggObj)
}

