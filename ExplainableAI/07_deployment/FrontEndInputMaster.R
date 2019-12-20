
 prepareDataForFrontEnd <- function(inPathPreparedData = "03_computedData/04_preparedData/",
                                    outPathTmp="03_computedData/07_deploymentData/",
                                    srcPathPyJsonPicker,
                                    outPathFinal = "03_computedData/07_deploymentData/finalJsons/"){
   
   ####  create json for 4D plot  -----
   createJsonFor4Dplot(inPathPreparedData,outPathTmp)


   
   ####  exec cmd line from R  -----
   # function(srcPathPyJsonPicker = "/05_src/07_deployment/") {
   #   
   # }
   cmd <- paste(paste0("cd ", getwd(), srcPathPyJsonPicker, ";"),
                "python3 json_picker.py",
                sep = '')
   system(command = cmd)
   
   #### dt to json  -----

   filterObs <- read.fst(paste0(inPathPreparedData, "FeatureDTsOptions.fst"), as.data.table = T)
   filterJsonDT <- filterDTtoJSON(filterObs)
   jsonlite::stream_out(x = filterJsonDT, 
                        con = file(description = paste0(outPathFinal,"filters.json")), 
                        prefix = ",")

 }
 