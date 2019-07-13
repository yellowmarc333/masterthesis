source("05_src/Sourcer.R")
sourcedFiles = sourceAll()

# init directory
computedDataPath <- "03_computedData/"

# 01 IMPORT
importData(inPath = "02_initialData/", 
           outPath = paste0(computedDataPath,"01_importedData/"))

#02 DATA CLEANING
cleanData(inPath = paste0(computedDataPath,"01_importedData/"),
          outPath = paste0(computedDataPath, "02_cleanedData/"))


# # 03 INTEGRATION
integrateData(inPath = "03_computedData/02_cleanedData/", 
             outPath = "03_computedData/03_integratedData/",
             trainSize = 0.6)

# 04 DATA PREPARATION
prepareDataBOW(inPath = "03_computedData/03_integratedData/",
            outPath = "03_computedData/04_preparedData/", 
            subsetSize = "10pc", mergeSD = FALSE)

prepareDataW2V(inPath = "03_computedData/03_integratedData/",
               outPath = "03_computedData/04_preparedData/", 
               subsetSize = "10pc", word2VecSize = 50)

prepareDataTFIDF(inPath = "03_computedData/03_integratedData/",
               outPath = "03_computedData/04_preparedData/", 
               subsetSize = "10pc")

prepareDataEmb(inPath = "03_computedData/03_integratedData/",
                 outPath = "03_computedData/04_preparedData/", 
                 subsetSize = "100pc")
