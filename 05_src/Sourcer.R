sourceAll = function() {
  # source external libraries
  require(wdl)
  require(fst)
  require(plotly)
  require(data.table)
  require(xgboost)
  require(stringi)
  require(stringr)
  require(rpart)
  require(rpart.plot)
  require(RcppRoll)
  require(Hmisc)
  require(ggplot2)
  require(readxl)
  library(gsubfn)
  require(assertthat)
  require(caret)
  require(DALEX)
  require(ceterisParibus)
  require(ranger)
  require(devtools)
  require(rjson)
  require(jsonlite)
  require(plyr)
  require(quanteda)
  require(topicmodels)
  library(tidytext)


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