cleanData = function(inPath = "03_computedData/01_importedData/", 
                     outPath = "03_computedData/02_cleanedData/"){

  data = fread(paste0(inPath, "News.csv"))
  
  headlineTmp <- as.character(data$headline)
  # lower
  headlineTmp <- tolower(headlineTmp)
  
  headlineTmp <- sapply(as.list(headlineTmp), cleanTerms)

  # converting column types
  data[, headline := headlineTmp]
  data[, category := tolower(as.character(category))]
  data[, authors := as.character(authors)]
  data[, link := as.character(link)]
  data[, short_description := as.character(short_description)]
  data[, date := as.Date(date)]
  
  # merge the categories together
  data[category == "the worldpost", category := "worldpost"]
  data[category == "parenting", category := "parents"]
  data[category == "culture & arts", category := "arts & culture"]
  data[category == "arts", category := "arts & culture"]
  data[category == "style", category := "style & beauty"]
  data[category == "green", category := "green & environment"]
  data[category == "environment", category := "green & environment"]
  
  
  # compression argument prevents converting error from o to c
  write.fst(data, path = paste0(outPath, "News.fst"), compress = 0)
}