# evaluation/explaining

# exchange words and see what happens, if the prediction gets into nearest
# input sequence of text (first first word, then 2nd word etc)
# class
# lime auschecken
# check human classification
# schaue benachbarte klassen an, in die am meisten falsch klassifiziert wird.

evaluateData <- function(inPath = "03_computedData/05_modelData/", 
                         outPath = "03_computedData/06_evaluatedData/",
                         subfolder) {
  
  assertString(inPath)
  assertString(outPath)
  
  if(!missing(subfolder)){
    assertString(subfolder)
    inPath = paste0(inPath, subfolder)
  }
  
  allModels <- list.files(path = inPath)
  
  resList <- lapply(allModels[3:7], function(x) {
    getModelMetrics(fileName = x)
  })
  
  res <- rbindlist(resList)
  
  write.fst(res, path = paste0(outPath, "evaluationResult.fst"))
}