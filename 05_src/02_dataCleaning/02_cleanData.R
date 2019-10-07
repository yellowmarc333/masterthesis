cleanData = function(inPath = "03_computedData/01_importedData/", 
                     outPath = "03_computedData/02_cleanedData/"){

  data = read.fst(path = paste0(inPath, "News.fst"), as.data.table = T)
  
  headlineTmp <- as.character(data$headline)
  # lower
  headlineTmp <- tolower(headlineTmp)
  
  headlineTmp <- sapply(as.list(headlineTmp), cleanTerms)

  # converting column types
  data[, headline := headlineTmp]
  data[, category := as.character(category)]
  data[, authors := as.character(authors)]
  data[, link := as.character(link)]
  data[, short_description := as.character(short_description)]
  data[, date := as.Date(date)]
  
  # compression argument prevents converting error from o to c
  write.fst(data, path = paste0(outPath, "News.fst"), compress = 0)
}