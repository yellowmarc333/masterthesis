cleanData = function(inPath = "03_computedData/01_importedData/", 
                     outPath = "03_computedData/02_cleanedData/"){

  data = read.fst(path = paste0(inPath, "News.fst"), as.data.table = T)
  
  notFound <- fread("03_computedData/02_cleanedData/notFound.csv")
  # reduce to those words where they have exactly only one "c"
  notFound <- notFound[grepl("c{1}", term, fixed = FALSE) & 
                         !grepl("c{2}", term, fixed = FALSE) , ]

  # headline transformation, this has to happen here (tolower is buggy)
  headlineTmp <- as.character(data$headline)
  headlineTmp <- tolower(headlineTmp)
  
  headlineTmp <- sapply(as.list(headlineTmp), cleanTerms)

  for (term in notFound$term) {
    termReplace <- gsub("c", "o", x = term, fixed = TRUE)
    headlineTmp <- gsub(x = headlineTmp, pattern = term,
                        replacement = termReplace,
                   fixed = TRUE)
  }


  data[, headline := headlineTmp]
  data[, category := as.character(category)]
  data[, authors := as.character(authors)]
  data[, link := as.character(link)]
  data[, short_description := as.character(short_description)]
  data[, date := as.Date(date)]
  
  # replace some terms that are later not found by global word vectors
  
  write.fst(data, path = paste0(outPath, "News.fst"))
}