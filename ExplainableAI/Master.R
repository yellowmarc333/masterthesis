source("05_src/Sourcer.R")
sourcedFiles = sourceAll()
setPlotParams()

#######
# 01 IMPORT
importData(inPath = "02_initialData/", 
           outPath = "03_computedData/01_importedData/")
#######
# 02 DATA CLEANING
cleanData(inPath = "03_computedData/01_importedData/", 
          outPath = "03_computedData/02_cleanedData/")

#######
# 05 Models

# calculating regression case
dir.create("03_computedData/05_modelData/regression/", showWarnings = FALSE,
           recursive = TRUE)
modelData(dataPath = "03_computedData/02_cleanedData/regression/",
          targetPath = "03_computedData/05_modelData/regression/",
          mode = "onlyTrain",
          sampling = "none",
          numberOfFolds = 5, 
          dataSets = list("all"))

# calculating timeseries case
dir.create("03_computedData/05_modelData/timeseries/", showWarnings = FALSE,
           recursive = TRUE)
modelData(dataPath = "03_computedData/02_cleanedData/timeseries/",
          targetPath = "03_computedData/05_modelData/timeseries/",
          mode = "onlyTrain",
          sampling = "none",
          numberOfFolds = 5, 
          dataSets = list("all"))


# regression case
dir.create("03_computedData/07_deploymentData/regression/", 
           showWarnings = FALSE, recursive = TRUE)
explainModel(dataPath = "03_computedData/05_modelData/regression/",
            targetPath = "03_computedData/07_deploymentData/regression/",
            n_feat = 7, 
            onlyFeatImp = FALSE,
            n_explain = 10, 
            feature_importance = TRUE, simple_model = TRUE,
            single_plots = TRUE, pdp_tupel_plots = TRUE,
            single_ceterisParibus = TRUE, 
            multiple_ceterisParibus = TRUE, waterfall = TRUE)

# timeseries case
dir.create("03_computedData/07_deploymentData/timeseries/", 
           showWarnings = FALSE, recursive = TRUE)
explainModel(dataPath = "03_computedData/05_modelData/timeseries/",
             targetPath = "03_computedData/07_deploymentData/timeseries/",
             n_feat = 10, 
             onlyFeatImp = FALSE,
             n_explain = 20, single_plots = FALSE)



