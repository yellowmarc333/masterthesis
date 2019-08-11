#' @author Marc Schmieder
#' @description imports all data files and writes them as fst files 
#' @param inPath directory in which the data is located
#' @param outPath target dir of imported data
#' @return Nothing. writes files as fst
importData = function(inPath = "02_initialData/", 
                      outPath = "03_computedData/01_importedData/"){
  assertString(inPath)
  assertString(outPath)
  
  fileName <- paste0(inPath, "News_Category_Dataset_v2.json")
  # open connection and read in textfile by lines
  con <- file(fileName, "r", encoding = "UTF-8") 
  input <- base::readLines(con, -1L, encoding = "UTF-8") 
  close(con) 
  
  data <- ldply(lapply(input, function(x) t(unlist(fromJSON(x)))))
  data <- as.data.table(data)

  #replace some symbols that are not utf-8 encoding and lead to encoding errors
  data[, headline := gsub(pattern = "’", replacement = "'",
                          x = headline, fixed = TRUE )]
  data[, headline := gsub(pattern = "‘", replacement = "'",
                          x = headline, fixed = TRUE )]
  data[, headline := gsub(pattern = "′", replacement = "'",
                          x = headline, fixed = TRUE )]
  data[, headline := gsub(pattern = "”", replacement = "'",
                          x = headline, fixed = TRUE )]
  data[, headline := gsub(pattern = "“", replacement = "'",
                          x = headline, fixed = TRUE )]
  data[, headline := gsub(pattern = "―", replacement = "-",
                          x = headline, fixed = TRUE )]
  data[, headline := gsub(pattern = "—", replacement = "-",
                          x = headline, fixed = TRUE )]
  data[, headline := gsub(pattern = "–", replacement = "-",
                          x = headline, fixed = TRUE )]
  data[, headline := gsub(pattern = "‐", replacement = "-",
                          x = headline, fixed = TRUE )]
  data[, headline := gsub(pattern = "-", replacement = "-",
                            x = headline, fixed = TRUE )]
  data[, headline := gsub(pattern = "…", replacement = "...",
                          x = headline, fixed = TRUE )]
  data[, headline := gsub(pattern = "\u2028", replacement = "",
                          x = headline, fixed = TRUE )]
  data[, headline := gsub(pattern = "  ", replacement = " ",
                          x = headline, fixed = TRUE )]
  data[, headline := gsub(pattern = " ", replacement = " ",
                          x = headline, fixed = TRUE )]
  data[, headline := gsub(pattern = "S", replacement = "S",
                          x = headline, fixed = TRUE )]
  data[, headline := gsub(pattern = "A", replacement = "A",
                          x = headline, fixed = TRUE )]
  data[, headline := gsub(pattern = "C", replacement = "C",
                          x = headline, fixed = TRUE )]
  data[, headline := gsub(pattern = "O", replacement = "C",
                          x = headline, fixed = TRUE )]
  data[, headline := gsub(pattern = "é", replacement = "e",
                          x = headline, fixed = TRUE )]
  data[, headline := gsub(pattern = "™", replacement = "Trademark",
                          x = headline, fixed = TRUE )]
  data[, headline := gsub(pattern = "ö", replacement = "oe",
                          x = headline, fixed = TRUE )]
  data[, headline := gsub(pattern = "ä", replacement = "ae",
                          x = headline, fixed = TRUE )]
  data[, headline := gsub(pattern = "ü", replacement = "ue",
                          x = headline, fixed = TRUE )]
  data[, headline := gsub(pattern = "Â", replacement = "A",
                          x = headline, fixed = TRUE )]
  

  fwrite(x = data, file = paste0(outPath, "News.csv"))
  write.fst(data, path = paste0(outPath, "News.fst"), uniform_encoding = TRUE)
}