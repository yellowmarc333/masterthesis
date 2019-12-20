
filterDTtoJSON <- function(filterObs) {
  # write columns that later will be options as list to allow multiple values in json
  filterObs[,Material := as.list(Material)]
  filterObs[,Trail := as.list(Trail)]
  # translate first level entries 
  filterFirstLevel <-  filterObs[, .(name = filterNo, 
                                     value = paste0("filter",str_pad(filterNo, width = 2, pad = "0")), 
                                     observations = nObs, 
                                     machines = ifelse(FeatureGroup == "all" | FeatureGroup == "withoutTS" , "all", "none"), 
                                     times = ifelse(FeatureGroup == "all" | FeatureGroup == "onlyTS" , "all", "none"))]
  # translate 2nd level entries 
  filterSecondLevelRaw <- filterObs
  filterSecondLevelRaw <- data.table::melt(data = filterObs[, .(filterNo, Material, Trail)], measure.vars = c("Material", "Trail"), variable.name = "name", value.name = "options")
  filterSecondLevelRaw <- filterSecondLevelRaw[!options == "all", ]
  filterSecondLevel <- filterSecondLevelRaw[, .(filters =list(.SD)), by= .(filterNo)]
  # filterSecondLevel <- filterSecondLevelRaw[, .(filters =list(list(.SD))), by= .(filterNo)]
  
  # build final DT that will be transformed to json 
  setnames(filterSecondLevel, "filterNo", "name")
  filterTojsonDT <- filterSecondLevel[filterFirstLevel, on =.(name)]
  setcolorder(filterTojsonDT, c("name","value", "observations", "machines", "times","filters"))
  # make sure filters with no options are empty lists
  for(i in 1: nrow(filterTojsonDT)){
    lengthFilter <- length(filterTojsonDT[i,]$filters[[1]])
    if(lengthFilter == 0) {
      # filterTojsonDT[i, noFilter := T]
      filterTojsonDT[i,]$filters[[1]] <- list(list())
    } else {
      # filterTojsonDT[i, noFilter := F]
    }
  }
  return(filterTojsonDT)
}

createJsonFor4Dplot <- function(inPath4DPlots = "03_computedData/04_preparedData/",
                                outPath = "03_computedData/07_deploymentData/"){
  # get file list
  fileList=list.files(path=inPath4DPlots)
  
  # filter after "filter" and ".fst"
  boolFilter = sapply(fileList, function(x){return(ifelse(length(grep("filter",x))==1,T,F))})
  jFileList = fileList[as.vector(boolFilter) & endsWith(fileList,".fst")]
  
  # loop over the files
  for (jFile in jFileList){
    # get filtername
    filter=gsub(".fst","",unlist(strsplit(jFile, "_"))[2])
    
    # read FeatureWithLabelCleaned
    FeatureWithLabelCleaned = read_fst(paste0(inPath4DPlots, jFile), as.data.table = T)
    
    # get only numerics for 4D plot
    colNames = names(FeatureWithLabelCleaned)
    colNumerics = sapply(FeatureWithLabelCleaned,is.numeric)
    colNames = colNames[colNumerics]
    feature4DPlot = FeatureWithLabelCleaned[,colNames,with=F]
    
    # build path for storing
    filterNo=str_pad(gsub("filter","",filter),2,pad="0")
    storePath=paste0(outPath,"filter",filterNo)
    dirList=list.dirs(path=storePath,recursive = F)
    for (dir in dirList){
      # create new directory
      path=paste0(dirList,"/features4D/")
      dir.create(path,recursive = T,showWarnings = F)
      
      # write FeatureWithLabelCleaned as json
      write_lines(toJSON(feature4DPlot),paste0(path,"FeatureWithLabelCleaned_",filter,".json"))
    }
  }
  
  return(1)
}
