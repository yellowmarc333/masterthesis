integrateData = function(inPath = "03_computedData/02_cleanedData/", 
                          outPath = "03_computedData/03_integratedData/",
                         trainSize = 0.4) {
  assertString(inPath)
  assertString(outPath)

  data <- read.fst(paste0(inPath, "News.fst"), as.data.table = TRUE)
  # merge the categories together
  data[category == "THE WORLDPOST", category := "WORLDPOST"]
  data[category == "PARENTING", category := "PARENTS"]
  data[category == "CULTURE & ARTS", category := "ARTS & CULTURE"]
  data[category == "ARTS", category := "ARTS & CULTURE"]
  data[category == "STYLE", category := "STYLE & BEAUTY"]
  data[category == "GREEN", category := "GREEN & ENVIRONMENT"]
  data[category == "ENVIRONMENT", category := "GREEN & ENVIRONMENT"]
  # set the ratio that the estimated amount of data points in the smallest 
  # class is 100.
  valSize <- round(nrow(data) / (min(table(data$category)) / 100))
  set.seed(100)
  valIndexes <- sample.int(nrow(data), valSize)
  valData <- data[valIndexes]
  
  # sample subsets and train indexes for 1, 10, 100 percent
  trainSubset100pc <- data[-valIndexes]

  set.seed(100)
  trainSubset1pc <- trainSubset100pc[sample.int(.N, size = round(.N * 0.01))]

  set.seed(100)
  trainSubset10pc <- trainSubset100pc[sample.int(.N, size = round(.N * 0.1))]
 
  write.fst(trainSubset100pc, path = paste0(outPath, "trainSubset100pc.fst"),
            compress = 0)
  write.fst(trainSubset10pc, path = paste0(outPath, "trainSubset10pc.fst"),
            compress = 0)
  write.fst(trainSubset1pc, path = paste0(outPath, "trainSubset1pc.fst"),
            compress = 0)
  
  write.fst(valData, path = paste0(outPath, "valData.fst"),
            compress = 0)
}