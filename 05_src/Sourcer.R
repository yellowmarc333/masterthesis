sourceAll = function() {
  # source external libraries
  require(xtable)
  require(fst)
  require(plotly)
  require(data.table)
  require(stringi)
  require(stringr)
  require(ggplot2)
  require(checkmate)
  require(jsonlite)
  require(devtools)
  require(plyr)
  require(dplyr)
  require(quanteda)
  require(xgboost)
  require(microbenchmark)
  require(ranger)
  require(text2vec)
  require(keras)
  require(tensorflow)
  require(ggwordcloud)
  require(ggpubr)
  require(LiblineaR)
  require(naivebayes)
  require(caret)
  require(ggrepel)
  require(RColorBrewer)
  require(wordcloud)
  require(wordcloud2)
  
  # source internal source files
  sourcefiles = c("05_src/00_exploration/explorationHelper.R",
                  "05_src/01_import/01_importMaster.R",
                  "05_src/02_dataCleaning/02_cleanData.R",
                  "05_src/02_dataCleaning/cleaningHelper.R",
                  "05_src/03_integration/integrateData.R",
                  "05_src/04_dataPreparation/04_textProcessingHelper.R",
                  "05_src/04_dataPreparation/prepareData.R",
                  #"05_src/04_dataPreparation/prepareDataOld.R",
                  
                  "05_src/05_model/modelPredictors.R",
                  "05_src/05_model/generalizedSampling.R",
                  "05_src/06_evaluation/evaluationHelper.R",
                  "05_src/06_evaluation/evaluationMaster.R",
                  
                  "05_src/09_utilities/oneHotEncode.R")
  
  for (file in sourcefiles) {
    print(paste("Sourcing file", file))
    source(file)
  }
  return(sourcefiles)
}
