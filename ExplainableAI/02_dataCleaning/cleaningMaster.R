#' @author Marc Schmieder
#' @description/disclaimer 
#' This function should save rds files with content:
#' train, trainLabel, test, labelName and type ("regression" or "timeseries")
#' Hardcoded readin and subsetting in for rossmann and boston housing datasets
#' @param dataPath directory in which the data is located
#' @param targetPath directory for saving modelled data
#' @return Nothing. writes files as rds
cleanData <- function(inPath, outPath) {
  assertString(inPath)
  assertString(outPath)

  # timeseries example dataset
  ts <- readRDS(paste0(inPath, "ts.rds"))

  tsCleaned <- sapply(ts, function(x) {
    x <- cleanColnames(x, verbose = FALSE)
    x <- cleanNaNanInf(x)
    return(x)
    })

  dt <- tsCleaned[["train"]]

  setnames(dt, "Date", "ds")
  dt[, ds := as.Date(ds)]
  setorderv(dt, "ds")
  #StoreSummary <- dt1[, .(count = sum(Customers)), by = Store]
  # setting to store = 733 (with the highest costumers)
  dt <- dt[Store == 733]

  train <- dt[1:(.N - 364), ]
  test <- dt[(.N-364):.N]
  
  trainLabel <- train[, Sales]
  train[, Sales := NULL]
  testLabel <- test[, Sales]
  test[, Sales := NULL]
  
  tsCleaned <- list(train = train,
                    trainLabel = trainLabel,
                    test = test,
                    testLabel = testLabel,
                    labelName = "Sales",
                    type = "timeseries")
  dir.create(paste0(outPath, "timeseries"), showWarnings = FALSE, 
             recursive = TRUE)
  saveRDS(tsCleaned, file = paste0(outPath, "timeseries/", "tsCleaned.rds"))
  
  # regression example dataset
  reg <- readRDS(paste0(inPath, "reg.rds"))
  reg <- reg[complete.cases(reg)]
  reg <- cleanColnames(reg)
  
  setnames(reg, colnames(reg), 
           c("Kriminalitaet", "AnteilWohnland", "AnteilIndustrie",
             "AnliegendAnFluss", "KonzentrationNitricOxid", "AnzahlRaeume",
             "AnteilEigentuemerVor1940", "EntfernungArbeitsAgenturen",
             "ZugaenglichkeitAutobahn", "Steuerrate", "SchuelerLehrerRatio",
             "AnteilMigration", "AnteilArbeitsschicht",
             "EigentumsWert"))
  reg <- removeMissingColumns(reg)
  reg <- removeLowInfoColumns(reg)
  reg <- cleanNaNanInf(reg)
  ind <- sample.int(nrow(reg), size = round(nrow(reg) * 0.8))
  train <- reg[ind]
  trainLabel <- train[, EigentumsWert]
  train[, EigentumsWert := NULL]
  test <- reg[-ind]
  test[, EigentumsWert := NULL]

  regCleaned <- list(train = train,
                    trainLabel = trainLabel,
                    test = test,
                    labelName = "EigentumsWert",
                    type = "regression")
  dir.create(paste0(outPath, "regression"), showWarnings = FALSE, 
             recursive = TRUE)
  saveRDS(regCleaned, file = paste0(outPath, "regression/", "regCleaned.rds"))
}

