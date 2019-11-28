source("05_src/Sourcer.R")
sourcedFiles = sourceAll()
dataPath <- "03_computedData/04_preparedData/"

allFiles <- list.files(path = "05_src/", recursive = T, full.names = T)
subFiles <- str_subset(allFiles, "[.][R]$") 
lineSum <- sum(sapply(subFiles, function(x) {
  length(readLines(x))
} )); lineSum



# increase vector memory
R_MAX_VSIZE = 50 * 10^9 
R_MAX_NUM_DLLS = 500
Sys.setenv('R_MAX_VSIZE'= R_MAX_VSIZE, "R_MAX_NUM_DLLS" = R_MAX_NUM_DLLS)
print(paste0("Max vector memory is set to ", as.numeric((Sys.getenv('R_MAX_VSIZE'))) / (10^9), " Gb"))


# 01 IMPORT
importData(inPath = "02_initialData/", 
           outPath = "03_computedData/01_importedData/")

#02 DATA CLEANING
cleanData(inPath = "03_computedData/01_importedData/",
          outPath = "03_computedData/02_cleanedData/")

# # 03 INTEGRATION
integrateData(inPath = "03_computedData/02_cleanedData/", 
             outPath = "03_computedData/03_integratedData/",
             trainSize = 0.8)

# 04 DATA PREPARATION
# this case works
prepareDataBOW(inPath = "03_computedData/03_integratedData/",
               outPath = "03_computedData/04_preparedData/", 
               subsetSize = "10pc", saveSparse =  FALSE,  mergeSD = FALSE)

prepareDataBOW(inPath = "03_computedData/03_integratedData/",
               outPath = "03_computedData/04_preparedData/", 
               subsetSize = "10pc", saveSparse =  TRUE,  mergeSD = FALSE)

prepareDataTFIDF(inPath = "03_computedData/03_integratedData/",
                 outPath = "03_computedData/04_preparedData/", 
                 subsetSize = "10pc", saveSparse = FALSE)

prepareDataTFIDF(inPath = "03_computedData/03_integratedData/",
                 outPath = "03_computedData/04_preparedData/", 
                 subsetSize = "10pc", saveSparse = TRUE)

prepareDataW2V(inPath = "03_computedData/03_integratedData/",
               outPath = "03_computedData/04_preparedData/", 
               subsetSize = "10pc", word2VecSize = 50)

prepareDataSeq(inPath = "03_computedData/03_integratedData/",
               outPath = "03_computedData/04_preparedData/", 
               subsetSize = "10pc", mergeSD = FALSE)


prepareDataGlove(inPath = "03_computedData/03_integratedData/",
                 outPath = "03_computedData/04_preparedData/", 
                 subsetSize = "10pc", word2VecSize = 50, mergeSD = FALSE)


# binary
resultBinary2 <- pipelineEmbBinary(inPath = "03_computedData/03_integratedData/",
                                  outPath = "03_computedData/04_preparedData/", 
                                  subsetSize = "100pc",
                                  binary = TRUE)
write.fst(resultBinary2, path = paste0("03_computedData/05_modelData/",
                                       "resultBinary2.fst"))




