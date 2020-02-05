evaluateData <- function(inPath = "03_computedData/05_modelData/", 
                         outPath = "03_computedData/06_evaluatedData/",
                         subfolder) {
  
  assertString(inPath)
  assertString(outPath)
  
  if(!missing(subfolder)){
    assertString(subfolder)
    inPath = paste0(inPath, subfolder)
  }
  
  allModels <- list.files(path = inPath)
  # subset to only .rds files
  allModels <- allModels[grepl(allModels, pattern = ".RDS", fixed = TRUE)]
  
  resList <- lapply(allModels, function(x) {
    getModelMetrics(fileName = x, path = inPath)
  })
  
  res <- rbindlist(resList)
  
  write.fst(res, path = paste0(outPath, "evaluationResult.fst"))
  return(res)
}



compareProbVsAcc <- function(inPath, cutN = 50) {
  assertString(inPath)

  allModels <- list.files(path = inPath)
  # subset to only .rds files
  allModels <- allModels[grepl(allModels, pattern = ".RDS", fixed = TRUE)]

  sequence <- seq(0, 1, length.out = cutN)
  beginn <- sequence[1: (cutN - 1)]
  end <- sequence[2: cutN]
  
  res <- data.table(cutLvl = 1:(cutN-1))
  
  for(fileName in allModels) {
    model <- readRDS(paste0(inPath, fileName))
    ProbAccDT <- model[["ProbAccDT"]]
    
    cutLvl <- sapply(ProbAccDT$Prob, function(w) {
      which(mapply(function(x, y, z) {
        between(z, x, y)
      }, beginn, end, w))
    }) 
    ProbAccDT[, cutLvl := cutLvl]
    
    probCutDT <- ProbAccDT[, .(CorrectMean = mean(Correct)), by = cutLvl]
    tmpName <- paste0(strsplit(fileName, split = "_")[[1]][2:3], collapse = ", ")
    setnames(probCutDT, "CorrectMean", tmpName)
    
    res <- merge(res, probCutDT, by = "cutLvl", all = TRUE)
  }

  setnames(res, c("cutLvl", "BOW, XGBoost","GloVe 300D, CNN" ,
            "GloVe 300D, Bi-LSTM", "SOW GloVe 300D, MLP"))
  plotData <- melt(res, id.vars = "cutLvl")
  plotData[, cutLvl := (cutLvl/cutN) - (1/cutN)/2]
  
  ggObj <- ggplot(plotData, aes(x = cutLvl, y = value,
                                color = variable, shape = variable)) + 
    geom_point(aes(shape = variable)) +
    geom_line(aes(color = variable)) +
    geom_abline(intercept = 0, slope = 1, colour = "black",
                linetype = "dashed", size = 0.5) +
    labs(x = "Mitte des Intervals der Modellwahrscheinlichkeit",
         y = "Anteil korrekt klassifizierter Beobachtungen") +
    scale_color_discrete(name = "Embedding, Modell", 
                         labels = c("BOW, XGBoost","GloVe 300D, CNN" ,
                                    "GloVe 300D, Bi-LSTM", 
                                    "SOW GloVe 300D, MLP")) +
    scale_shape_discrete(name = "Embedding, Modell", 
                         labels = c("BOW, XGBoost","GloVe 300D, CNN" ,
                                    "GloVe 300D, Bi-LSTM", 
                                    "SOW GloVe 300D, MLP")) +
    theme(axis.text.x  = element_text(size = 14),
          axis.text.y = element_text(size = 15),
          axis.title = element_text(size = 28),
          axis.ticks.x = element_line(linetype = "solid"),
          legend.background = element_rect(fill = "lightgrey", size = 6),
          legend.key = element_rect(fill = "white",
                                    color = NA, size = 8),
          legend.position = "top",
          legend.text = element_text(size = 17),
          legend.title = element_text(size = 19),
          panel.grid.major = element_line(), 
          panel.grid.minor = element_line()) ; ggObj
  
  return(ggObj)
}


getModelMetrics <- function(fileName, 
                            path = "03_computedData/05_modelData/") {
  assertString(fileName, pattern = ".RDS")
  assertString(path)
  
  model <- readRDS(paste0(path, fileName))
  assertList(model)
  assertSubset(x = c("confusionMatrix",
                     "predictions",
                     "testLabelRaw"),
               choices = names(model))

  # if the prediction is correct, how sure was the model
  probAccDT <- model$ProbAccDT
  probIfCorrect <- round(mean(probAccDT[Correct == TRUE, .(Prob)][[1]]),3)
  
  confMat <- model$confusionMatrix
  namesConfMat <- colnames(confMat)
  predictions <- model$predictions
  assertDataTable(predictions)
  testLabelRaw <- model$testLabelRaw
  categoryNames <- levels(testLabelRaw)
  C <- nrow(confMat)
  
  # calc basic measures
  # prevent that row or colsums are 0 (divided later)
  # quelle https://www.youtube.com/watch?v=HBi-P5j0Kec & wiki
  
  true_positives <- diag(confMat)
  false_positives <- colSums(confMat) - diag(confMat)
  false_negatives <- rowSums(confMat) - diag(confMat)

  # alternative schreibweise, checked!
  # true_negatives <- rep(sum(confMat), C) - (colSums(confMat) +
  #                                              rowSums(confMat) -
  #                                              diag(confMat))
  true_negatives <- rep(sum(confMat), C) - (false_positives +
                                               false_negatives +
                                              diag(confMat))
  # check of measures are calculated correctly
  assert(all(sum(confMat) == (true_positives + false_positives +
                             true_negatives + false_negatives)))
  
  accuracy <- round(sum(diag(confMat)) / sum(confMat),
                    digits = 3)
  accuracy_mean <- round(mean(diag(confMat) / rowSums(confMat)), 3)
  
  # compute zähler seperate in case that no observations get classified in
  # a class and thus there will be no division by 0
  precision_frac <- ifelse(true_positives + false_positives != 0,
                             true_positives + false_positives,
                             0.00001)
  recall_frac <- ifelse(true_positives + false_negatives != 0,
                          true_positives + false_negatives,
                          0.00001)

  # precision_mu and recall_mu are always the same. I checked
  # they are also the same as accuracy cause it gets divided by 
  # the sum of the whole confusion matrix
  precision_mu <- round(sum(true_positives) / 
                         sum(precision_frac),
                       digits = 3)
  recall_mu <- round(sum(true_positives) / 
                       sum(recall_frac),
                     digits = 3)
  precision_M <- round(sum(true_positives / 
                             precision_frac) / C,
                        digits = 3)
  recall_M <- round(sum(true_positives / 
                          recall_frac) / C ,
                     digits = 3)
  
  f1_mu <- round(2 * precision_mu * recall_mu / (precision_mu + recall_mu), 3)
  f1_M <- round(2 * precision_M * recall_M / (precision_M + recall_M), 3)
  
  # calc multiclassif logloss / categorical cross entropy
  colIndexes <- sapply(testLabelRaw, function(x) {
    which(x == categoryNames)
  })
  predictedLabel <- t(apply(predictions, 1, function(x) {
    return(names(x[which.max(x)]))
  }))
  truthProb <- predictions[, mapply(function(x, y) {
    .SD[y, x, with = FALSE][[1]]
  }, colIndexes, 1:length(testLabelRaw))]
  
  truthProb <- ifelse(truthProb == 0, 1/500, truthProb)
  
  # this is a check that the confusion matrix is the right way
  #sum((testLabelRaw == "black voices") & (predictedLabel == "entertainment"))
  
  # take mean of log loss, then can be compared with different sample sizes
  mlogloss <- round(mean(log(truthProb)) * (-1), 3)
  
  # occurs if no tree from random forest votes for a class
  #if(is.infinite(mlogloss)) 
  
  return(list(modelName = fileName,
              accuracy = accuracy,
              accuracy_mean = accuracy_mean,
              mlogloss = mlogloss,
              precision_mu = precision_mu,
              recall_mu = recall_mu,
              precision_M = precision_M,
              recall_M = recall_M,
              f1_mu = f1_mu,
              f1_M = f1_M,
              probIfCorrect = probIfCorrect))
  
}


plotAccByClass <- function(inPath) {
  assertString(inPath)
  
  allModels <- list.files(path = inPath)
  assert_character(allModels, min.len = 1)
  # subset to only .rds files
  allModels <- allModels[grepl(allModels, pattern = ".RDS", fixed = TRUE)]
 
  res <- data.table()
  res2 <- data.table()
  for(fileName in allModels) {
    model <- readRDS(paste0(inPath, fileName))
    data <- model[["accByClass"]]
    
    counts <- rowSums(model$confusionMatrix)
  
    res[, Var := data]
    res2[, Count := counts]
    tmpName <- paste0(strsplit(fileName, split = "_")[[1]][2:3], collapse = "_")
    setnames(res, "Var", tmpName)
    setnames(res2, "Count", tmpName)
  }
  

  res[, category := names(data)]
  res2[, category := names(data)]
  
  
  resMelted <- melt(res, id.vars = c("category"))
  resMelted2 <- melt(res2, id.vars = c("category"),
                      , value.name = "count")
  plotData <- merge(resMelted, resMelted2, 
                    by = c("category", "variable"))
  plotData[, Order := max(value), by = .(category)]
  # renaming for plotting (hardcode)
  plotData[variable == "BOW_XG", variable := "BOW, XGBoost"]
  plotData[variable == "GloveArray300_CNNArray", 
           variable := "GloVe 300D, CNN"]
  plotData[variable == "GloveArray300_LSTMArray", 
           variable := "GloVe 300D, Bi-LSTM"]
  plotData[variable == "GloveSums300_MLP", 
           variable := "SOW GloVe 300D, MLP"]
  
  ggObj <- ggplot(plotData, aes(x = reorder(category, -Order),
                                    y = value, fill = variable, 
                                    label = category)) +
    geom_bar(stat = "identity", position = "dodge") + 
    labs(x = "Nachrichtenkategorie",
         y = "Accuracy",
         fill = "Embedding, Modell: ") +
    geom_text(aes(label = round(value, 2)), 
              position = position_dodge(width = 0.9),
               vjust = 0.5, hjust = 1.4,
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


identifyNeighborClasses <- function(inPath) {
  assertString(inPath)
  
  allModels <- list.files(path = inPath)
  # subset to only .rds files
  allModels <- allModels[grepl(allModels, pattern = ".RDS", fixed = TRUE)]
  
  res <- list()
  
  for(j in 1:length((allModels))) {
    model <- readRDS(paste0(inPath, allModels[j]))
    data <- model[["confusionMatrix"]]
    trueClasses <- row.names(data)
    neighborCount <- list()
    for(i in 1:nrow(data)) {
      x <- data[i, ]
      x[i] <- 0 # setting the diagonal = 0
      sorted <- sort(x, decreasing = TRUE)
      highest1 <- sorted[1]
      highest2 <- sorted[2]
      NC1 <- paste0("textit{", names(which(x == highest1)[1])
                    , "}, ($", highest1, "$)")
      # Marc: outcomment, maybe for Anhang
      # NC2 <- paste0("textit{", names(which(x == highest2)[1])
      #               , "}, ($", highest2, "$)")
      
      neighborCount[[i]] <- list(trueClass = trueClasses[i],
                                 NC1 = NC1
                                 #,
                                 #NC2 = NC2
                                 )
    }
    res[[allModels[j]]] <- neighborCount
  }

  resListed <- lapply(res, function(x) rbindlist(x))

  
  resDT <- data.table(trueClass = resListed[[1]]$trueClass)
  for(i in seq_along(resListed)) {
    tmpName <- names(resListed)[i]
    setnames(resListed[[i]], "NC1", paste0(tmpName, "_NC1"))
    #setnames(resListed[[i]], "NC2", paste0(tmpName, "_NC2"))
    resDT <- merge(resDT, resListed[[i]], by = "trueClass")
  }
  return(resDT)
}


analyseCNNFilters <- function(modelPath, WEPath, n_gram = 2) {
  assertString(modelPath)
  assertString(WEPath)
  
  data <- readRDS(modelPath)
  WE <- t(read.fst(WEPath, as.data.table = TRUE))

  weights <- data$weights
  
  filterLevel <- weights[[n_gram*2 -3]]
  
  
  resFinal <- list()
  combn <- combn(ncol(WE), n_gram, simplify = FALSE)
  # n <- 1000000
  # combnRed <- combn[1:n]
  
  for(i in 1:dim(filterLevel)[3]){
    print(paste("calculation filter", i))
    # selecting filter
    filter <- filterLevel[, , i]
    # lapply > vapply > sapply
    res <- lapply(combn, function(x) {
      tmpWE <- WE[x,]
      res <- sum(filter * tmpWE)
      resNames <- paste0(row.names(tmpWE), collapse = " , ")
      return(list(res = res,
                  resNames = resNames))
    })
    resDT <- rbindlist(res)
    resFinal[[i]] <- resDT
  }

  return(resFinal)
}

