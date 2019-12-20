importData <- function(inPath, outPath) {
  assertString(inPath)
  assertString(outPath)
  
  dt1 <- fread(paste0(inPath, "timeseries/", "train.csv"))
  dt2 <- fread(paste0(inPath, "timeseries/", "test.csv"))
  dt3 <- fread(paste0(inPath, "timeseries/", "store.csv"))
  
  ts <- list(train = dt1, 
             test = dt2,
             store = dt3)
  saveRDS(ts, file = paste0(outPath, "ts.rds"))
  
  dt1 <- fread(paste0(inPath, "regression/HousingData.csv"))
  saveRDS(dt1, file = paste0(outPath, "reg.rds"))
}

