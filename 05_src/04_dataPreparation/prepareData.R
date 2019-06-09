prepareData = function(inPath = "03_computedData/02_dataCleaning/", 
                       outPath = "03_computedData/04_preparedData/",
                       subsetSize = 0.01){
  data = read.fst(path = paste0(inPath, "News.fst"), as.data.table = T)
  set.seed(123)
  subsetData = data[sample.int(.N, floor(.N * subsetSize))]

  label = oneHotEncode(subsetData$category)
  labelRaw = as.factor(subsetData$category)
  texts = subsetData$headline
  tokens <- processTokens(texts)
  
  # reduce to tokens > 0
  hasWords <- lengths(tokens) > 0
  tokens <- tokens[hasWords]
  label = label[hasWords]
  labelRaw = labelRaw[hasWords]
  
  tokens.dfm <- dfm(tokens)
  
  tokens.dfm <- quanteda::dfm_trim(tokens.dfm, min_docfreq = 5)
  tokens.dt <- as.data.table(quanteda::convert(tokens.dfm, to = "data.frame"))
  tokens.dt[, document := NULL]
  tokens.dt.label <- data.table(labelRaw, tokens.dt)
  
  write.fst(label, path = paste0(outPath, "NewsLabel", subsetSize,
                                 "Subset.fst"))
  write.fst(tokens.dt, path = paste0(outPath, "News", subsetSize,
                                     "Subset.fst"))
  write.fst(tokens.dt.label, path = paste0(outPath, "NewsWithLabel", 
                                           subsetSize, "Subset.fst"))
}