wdl::installDependencies()
source("05_src/Sourcer.R")
sourcedFiles = sourceAll()
wdl::performChecks(sourcefiles = sourcedFiles, srcFolder = "05_src", 
                   ignoreFolders = c("src/00_deprecated", "src/06_results"), printCallHierarchy = T)
setPlotParams()


##


#######
# increase vector memory
R_MAX_VSIZE = 100 * 10^9 
R_MAX_NUM_DLLS = 500
Sys.setenv('R_MAX_VSIZE'= R_MAX_VSIZE, "R_MAX_NUM_DLLS" = R_MAX_NUM_DLLS)
print(paste0("Max vector memory is set to ", as.numeric((Sys.getenv('R_MAX_VSIZE'))) / (10^9), " Gb"))


#######
# init directory
computedDataPath <- "03_computedData/"
dir.create(computedDataPath, showWarnings = F)
folders <- c("01_importedData/", "02_cleanedData/", "03_integratedData/", "04_preparedData/", "05_modelData/","06_evaluatedData/", "07_deploymentData/finalJsons/")
for (folder in folders) {dir.create(paste0(computedDataPath, folder), recursive = T, showWarnings = F)}


#######
# 01 IMPORT
importData(inPath = "02_initialData/", 
           outPath = paste0(computedDataPath,"01_importedData/"))


#######
#02 DATA CLEANING
cleanData(inPath = paste0(computedDataPath,"01_importedData/"),
          outPath = paste0(computedDataPath, "02_cleanedData/"))

# 
# #######
# # 03 INTEGRATION
# integrateData(inPath = paste0(computedDataPath, "02_cleanedData/"), 
#               outPath = paste0(computedDataPath, "03_integratedData/"))
# 
# 
# ######
# 04 DATA PREPARATION
prepareData(inPath = paste0(computedDataPath, "02_cleanedData/"),
            outPath = paste0(computedDataPath, "04_preparedData/"))


# #######
# # 05 Models
# modelData(dataPath = paste0(computedDataPath,"04_preparedData/"),
#           targetPath = paste0(computedDataPath,"05_modelData/"),
#           mode = "fold",
#           numberOfFolds = 5, 
#           dataSets = list("all"))
# 
# 
# #######
# # 06 Evaluation 
# evaluateResults(dataPath = paste0(computedDataPath,"05_modelData/"),
#                 targetPath = paste0(computedDataPath,"06_evaluatedData/"), 
#                 threshold = "naive",
#                 verbose = T)
# 
# #######
# # 07 Deployment
# saveEvaluationFiles(dataPath = paste0(computedDataPath,"06_evaluatedData/"),
#                     targetPath = paste0(computedDataPath,"07_deploymentData/"))
# 
# explainData(dataPath = paste0(computedDataPath,"05_modelData/"),
#             targetPath = paste0(computedDataPath,"07_deploymentData/"),
#             mode = "fold",
#             n_feat = 10, 
#             onlyFeatImp = F,
#             n_explain = 20)
# 
# prepareDataForFrontEnd(inPathPreparedData = paste0(computedDataPath,"04_preparedData/"),
#                        outPathTmp= paste0(computedDataPath,"07_deploymentData/"),
#                        srcPathPyJsonPicker = paste0("/05_src/07_deployment/"),
#                        outPathFinal = paste0(computedDataPath,"07_deploymentData/finalJsons"))
# #######
# # 00 Exploration
