source("05_src/Sourcer.R")
sourcedFiles = sourceAll()
dataPath <- "03_computedData/04_preparedData/"


# increase vector memory
R_MAX_VSIZE = 50 * 10^9 
R_MAX_NUM_DLLS = 500
Sys.setenv('R_MAX_VSIZE'= R_MAX_VSIZE, "R_MAX_NUM_DLLS" = R_MAX_NUM_DLLS)
print(paste0("Max vector memory is set to ", as.numeric((Sys.getenv('R_MAX_VSIZE'))) / (10^9), " Gb"))

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
            subsetSize = "100pc", mergeSD = FALSE)

prepareDataTFIDF(inPath = "03_computedData/03_integratedData/",
               outPath = "03_computedData/04_preparedData/", 
               subsetSize = "10pc")

prepareDataW2V(inPath = "03_computedData/03_integratedData/",
               outPath = "03_computedData/04_preparedData/", 
               subsetSize = "10pc", word2VecSize = 50)

prepareDataEmb(inPath = "03_computedData/03_integratedData/",
                 outPath = "03_computedData/04_preparedData/", 
                 subsetSize = "10pc", mergeSD = TRUE)

prepareDataGlove(inPath = "03_computedData/03_integratedData/",
               outPath = "03_computedData/04_preparedData/", 
               subsetSize = "100pc", word2VecSize = 50, mergeSD = TRUE)


# binary
resultBinary2 <- pipelineEmbBinary(inPath = "03_computedData/03_integratedData/",
                                  outPath = "03_computedData/04_preparedData/", 
                                  subsetSize = "100pc",
                                  binary = TRUE)
write.fst(resultBinary2, path = paste0("03_computedData/05_modelData/",
                                       "resultBinary2.fst"))




