cleanData = function(inPath = "03_computedData/01_importedData/", 
                     outPath = "03_computedData/02_cleanedData/"){

  data = fread(paste0(inPath, "News.csv"))

  headlineTmp <- data$headline
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
  
  # that this still occurs is an fst problem
  # any(grepl(headlineTmp, pattern =  "â"))
  
  # merge the categories together
  data[category == "the worldpost", category := "worldpost"]
  data[category == "parenting", category := "parents"]
  data[category == "culture & arts", category := "arts & culture"]
  data[category == "arts", category := "arts & culture"]
  data[category == "style", category := "style & beauty"]
  data[category == "green", category := "green & environment"]
  data[category == "environment", category := "green & environment"]
  
  # removing empty points if there are any
  emptyPoints <- sapply(as.list(headlineTmp), function(x) {
    length(strsplit(x, split = " ")[[1]]) == 0
  })
  print(paste("there are", sum(emptyPoints), "empty points that will be removed"))
  
  data <- data[!emptyPoints]
  
  # compression argument prevents converting error from o to c
  write.fst(data, path = paste0(outPath, "News.fst"), compress = 0,
            uniform_encoding = FALSE)
  fwrite(data, file = paste0(outPath, "News.csv"))
}