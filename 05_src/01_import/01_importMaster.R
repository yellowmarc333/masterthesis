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
  con <- file(fileName, "r") 
  input <- base::readLines(con, -1L) 
  close(con) 
  
  data <- ldply(lapply(input, function(x) t(unlist(fromJSON(x))))) 
  write.fst(data, path = paste0(outPath, "News.fst"))
}