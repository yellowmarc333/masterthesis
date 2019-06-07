sourceAll = function() {
  # source external libraries
  require(wdl)
  require(fst)
  require(plotly)
  require(data.table)
  require(xgboost)
  require(stringi)
  require(stringr)
  require(RcppRoll)
  require(ggplot2)
  library(gsubfn)
  require(checkmate)
  require(caret)
  require(devtools)



  # source internal source files
  sourcefiles = c("05_src/01_import/01_importMaster.R",
                  "05_src/02_dataCleaning/02_cleanData.R",
                  
                  "05_src/04_dataPreparation/04_textProcessingHelper.R")
  
  for (file in sourcefiles) {
    print(paste("Sourcing file", file))
    source(file)
  }
  return(sourcefiles)
}