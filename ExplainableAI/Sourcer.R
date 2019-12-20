sourceAll = function() {
  # source external libraries
  require(Rcpp)
  require(lime)
  require(wdl)
  require(wdlViz)
  require(fst)
  require(plotly)
  require(checkmate)
  require(devtools)
  require(data.table)
  require(xgboost)
  require(stringi)
  require(rpart)
  require(ggplot2)
  require(DALEX)
  require(ceterisParibus)
  require(ranger)
  #install_github("AppliedDataSciencePartners/xgboostExplainer")
  require(xgboostExplainer)
  require(tree)
  require(LiblineaR)
  require(ggpubr)
  require(pdp)
  require(prophet)

  # source internal source files
  sourcefiles = c("05_src/01_import/ImportMaster.R", 
                  "05_src/02_dataCleaning/cleaningMaster.R",
                  "05_src/05_model/01_modellingMaster.R",
                  "05_src/05_model/10_predictionModels.R",
                  "05_src/05_model/11_predictionHelper.R",
                  "05_src/05_model/12_predictionFramework.R",
                  "05_src/05_model/13_missingValueHandling.R",
                  "05_src/07_deployment/01_deploymentMaster.R",
                  "05_src/07_deployment/11_deploymentHelper.R",
                  "05_src/07_deployment/FrontEndInputMaster.R",
                  "05_src/07_deployment/FrontEndInputHelper.R",
                  "05_src/07_deployment/simpleModels.R",
                  
                  "05_src/92_utilities/07_plottingFunctions.R",
                  "05_src/92_utilities/utilities.R"
  )
  
  for (file in sourcefiles) {
    print(paste("Sourcing file", file))
    source(file)
  }
  return(sourcefiles)
}