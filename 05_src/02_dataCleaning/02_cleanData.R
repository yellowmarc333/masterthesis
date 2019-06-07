cleanData = function(inPath = "03_computedData/01_importedData/", 
                     outPath = "03_computedData/02_cleanedData/"){
  
  data = read.fst(path = paste0(inPath, "News.fst"), as.data.table = T)
  data[, category := as.character(category)]
  data[, headline := as.character(headline)]
  data[, authors := as.character(authors)]
  data[, link := as.character(link)]
  data[, short_description := as.character(short_description)]
  data[, date := as.Date(date)]
  
  write.fst(data, path = paste0(outPath, "News.fst"))
}