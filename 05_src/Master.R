devtools::install_git("https://gitlab.com/wdlpackage/wdl.git",
  ref = "v2_Refactoring")
)

wdl::installDependencies()
source("05_src/Sourcer.R")
sourcedFiles = sourceAll()
wdl::performChecks(sourcefiles = sourcedFiles, srcFolder = "05_src", 
                   ignoreFolders = c("src/00_deprecated", "src/06_results"), 
                   printCallHierarchy = T)


#######
# init directory
computedDataPath <- "03_computedData/"

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


