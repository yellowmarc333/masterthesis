sourceAll = function() {
  # source external libraries
  require(fst)
  require(plotly)
  require(data.table)
  require(stringi)
  require(stringr)
  require(RcppRoll)
  require(ggplot2)
  require(checkmate)
  require(jsonlite)
  require(devtools)
  require(plyr)
  require(quanteda)
  require(h2o)
  require(nnet)
  require(neuralnet)



  # source internal source files
  sourcefiles = c("05_src/01_import/01_importMaster.R",
                  "05_src/02_dataCleaning/02_cleanData.R",
                  
                  "05_src/04_dataPreparation/04_textProcessingHelper.R",
                  "05_src/04_dataPreparation/prepareData.R",
                  "05_src/09_utilities/oneHotEncode.R")
  
  for (file in sourcefiles) {
    print(paste("Sourcing file", file))
    source(file)
  }
  return(sourcefiles)
}