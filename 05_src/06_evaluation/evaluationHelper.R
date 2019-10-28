compareProbVsAcc <- function(data, cutN = 100) {
  
  ProbAccDT <- data[["ProbAccDT"]]
  ProbAccDT[, ProbCut := cut(x = Prob, breaks = cutN)]
  
  plotDT <- ProbAccDT[, .(ProbMean = mean(Prob),
                          CorrectMean = mean(Correct)), by = ProbCut]
  setorderv(plotDT, "ProbCut", "1")
  plotDT[, CutLvl := 1: .N]
  
  ggObj <- ggplot(plotDT, aes(x = CutLvl)) + 
    geom_line(aes(y = CorrectMean), color = "green") +
    geom_line(aes(y = ProbMean), color = "blue")
  
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
  
  # calc basic measures
  # prevent that row or colsums are 0 (divided later)
  rowSums <- ifelse(rowSums(confMat) == 0, 0.000000001,
                    rowSums(confMat))
  colSums <- ifelse(colSums(confMat) == 0, 0.000000001,
                    colSums(confMat))
  
  accuracy <- round(sum(diag(confMat)) / sum(confMat),
                    digits = 3)
  precision <- round(mean(diag(confMat) / colSums),
                     digits = 3)
  recall <- round(mean(diag(confMat) / rowSums, ),
                  digits = 3)
  f1 <- 2 * precision * recall / (precision + recall)
  
  # calc multiclassif logloss / categorical cross entropy
  truthProb <- predictions[, mapply(function(x, y) {
    .SD[y, get(x, pos = -1, inherits = FALSE)]
  }, as.character(testLabelRaw), 1:length(testLabelRaw))]
  
  
  # take mean of log loss, then can be compared with different sample sizes
  mlogloss <- mean( log(truthProb)) * (-1)
  
  return(list(modelName = fileName,
              accuracy = accuracy,
              precision = precision,
              recall = recall,
              f1 = f1,
              mlogloss = mlogloss))
  
}

