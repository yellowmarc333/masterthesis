compareProbVsAcc <- function(inPath, cutN = 50) {
  assertString(inPath)
  
  browser()

  allModels <- list.files(path = inPath)
  # subset to only .rds files
  allModels <- allModels[grepl(allModels, pattern = ".RDS", fixed = TRUE)]
  
  data <- readRDS(paste0(inPath, allModels[3]))
  # browser()
  
  ProbAccDT <- data[["ProbAccDT"]]
  ProbAccDT[, ProbCut := cut(x = Prob, breaks = cutN)]
  
  plotDT <- ProbAccDT[, .(ProbMean = mean(Prob),
                          CorrectMean = mean(Correct)), by = ProbCut]
  setorderv(plotDT, "ProbCut", "1")
  plotDT[, CutLvl := 1: .N]
  
  ggObj <- ggplot(plotDT, aes(x = CutLvl)) + 
    geom_line(aes(y = CorrectMean), color = "darkgreen") +
    geom_line(aes(y = ProbMean), color = "blue"); ggObj
  
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
  true_negatives <- rep(sum(confMat), C) - (colSums(confMat) +
                                               rowSums(confMat) -
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
              f1_M = f1_M))
  
}


plotAccByClass <- function(inPath) {
  inPath <- "03_computedData/05_modelData/finalselection/"
  assertString(inPath)
  
  allModels <- list.files(path = inPath)
  # subset to only .rds files
  allModels <- allModels[grepl(allModels, pattern = ".RDS", fixed = TRUE)]
 
  res <- data.table()
  for(fileName in allModels) {
    model <- readRDS(paste0(inPath, fileName))
    data <- model[["accByClass"]]
    res[, Var := data]
    tmpName <- paste0(strsplit(fileName, split = "_")[[1]][2:3], collapse = "_")
    setnames(res, "Var", tmpName)
  }
  
  res[, category := names(data)]
  plotData <- melt(res, id.vars = "category")
  plotData[, Order := max(value), by = .(category)]
  
  ggObj <- ggplot(plotData, aes(x = reorder(category, -Order),
                                    y = value, fill = variable, 
                                    label = category)) +
    geom_bar(stat = "identity", position = "dodge") + 
    labs(x = "Nachrichtenkategorie",
         y = "Accuracy") +
    theme(axis.text.x  = element_text(angle = 45,
                                      vjust = 1, hjust = 1,
                                      size = 14),
          axis.text.y = element_text(size = 15),
          axis.title = element_text(size = 28),
          axis.ticks.x = element_line(),
          legend.background = element_rect(fill = "lightgrey"),
          legend.key = element_rect(fill = "lightblue", color = NA),
          legend.position = "top",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank());ggObj
  
  
  theme(axis.text.x  = element_text(angle = 45,
                                    vjust = 1, hjust = 1,
                                    size = 14),
        axis.title = element_text(size = 28),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        axis.ticks.x = element_line(),
        panel.background = element_blank(),
        legend.background = element_rect(fill = "lightgrey"),
        legend.key = element_rect(fill = "lightblue", color = NA),
        legend.position = "top",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
  
  return(ggObj)
}



