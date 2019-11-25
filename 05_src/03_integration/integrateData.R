integrateData = function(inPath = "03_computedData/02_cleanedData/", 
                          outPath = "03_computedData/03_integratedData/",
                         trainSize = 0.4) {
  assertString(inPath)
  assertString(outPath)

  data <- read.fst(paste0(inPath, "News.fst"), as.data.table = TRUE)

  # set the ratio that the estimated amount of data points in the smallest 
  # class is 100.
  valSize <- round(nrow(data) / (min(table(data$category)) / 100))
  set.seed(100)
  valIndexes <- sample.int(nrow(data), valSize)
  valData <- data[valIndexes]
  
  # sample subsets and train indexes for 1, 10, 100 percent
  trainSubset100pc <- data[-valIndexes]
  set.seed(100)
  indexes100pc <- sample.int(nrow(trainSubset100pc), 
                            floor(nrow(trainSubset100pc) * trainSize))

  set.seed(100)
  trainSubset10pc <- trainSubset100pc[sample.int(.N, size = round(.N * 0.1))]
  set.seed(100)
  indexes10pc <- sample.int(nrow(trainSubset10pc), 
                        floor(nrow(trainSubset10pc) * trainSize))
 
  write.fst(trainSubset100pc, path = paste0(outPath, "trainSubset100pc.fst"),
            compress = 0, uniform_encoding = FALSE)
  saveRDS(indexes100pc, file = paste0(outPath, "indexes100pc.rds"))
  write.fst(trainSubset10pc, path = paste0(outPath, "trainSubset10pc.fst"),
            compress = 0, uniform_encoding = FALSE)
  saveRDS(indexes10pc, file = paste0(outPath, "indexes10pc.rds"))

  
  write.fst(valData, path = paste0(outPath, "valData.fst"),
            compress = 0, uniform_encoding = FALSE)
}