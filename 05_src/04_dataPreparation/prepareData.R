prepareData = function(inPath = "03_computedData/02_dataCleaning/", 
                       outPath = "03_computedData/04_preparedData/",
                       subsetSize = 0.01){
  data = read.fst(path = paste0(inPath, "News.fst"), as.data.table = T)
  
  subsetData = data[sample.int(.N, .N * subsetSize)]
  
  label = wdl::oneHotEncode(subsetData$category)
  texts = subsetData$headline
  tokens <- processTokens(texts)
  hasWords <- lengths(tokens) > 0
  tokens <- tokens[hasWords]
  tokens.dfm <- dfm(tokens)
  
  tokens.dfm = quanteda::dfm_trim(tokens.dfm, min_docfreq = 5)
  tokens.dt = as.data.table(quanteda::convert(tokens.dfm, to = "data.frame"))
  
  write.fst(label, path = paste0(outPath, "NewsLabel", subsetSize, "Subset.fst"))
  write.fst(tokens.dt, path = paste0(outPath, "News", subsetSize, "Subset.fst"))
}