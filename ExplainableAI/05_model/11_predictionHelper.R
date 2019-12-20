#' @author Felix Kleine Bösing
#' @description return  list of list containing the indices for train and test split
#' @param label
#' @param folds
#' @return list of list containing validation indices
getListOfFolds <- function(label, folds) {
  valTrainIndices = list()
  if (folds == 1) {
    lenLabel = length(label)
    valIndices = list(sample.int(lenLabel, as.integer(0.2*lenLabel)))
  } else (
    valIndices = caret::createFolds(label, folds)
  )
  
  for (i in 1:length(valIndices)) {
    trainIndices = seq(1, length(label))[!seq(1, length(label)) %in% valIndices[[i]]]
    valTrainIndices[[paste0("Fold", i)]] = list(val = valIndices[[i]], train = trainIndices)
  }
  return(valTrainIndices)
}

#' @author Felix Kleine Bösing
#' @description  return  list of list containing the indices for train and test split
#' @param years vector of years from data
#' @return list of list containing validation indices
getIndicesByYears <- function(years) {
  valTrainIndices = list()
  uniqueYears = sort(unique(years))
  for (year in uniqueYears[2:length(uniqueYears)]) {
    valIndex = seq(1, length(years))[years == year]
    trainIndex = seq(1, length(years))[years < year]
    valTrainIndices[[as.character(year)]] = list(train = trainIndex,
                                   val = valIndex)
  }
  return(valTrainIndices)
}

#' @author Felix Kleine Bösing
#' @description calculates linear regression
#' @param years vector of years from data
#' @return list of list containing validation indices
getIndicesOnlyTrain <- function(label) {
  return(list(onlyTrain = list(train =seq(1, length(label)),
                               val = c())))
}

#' @author Marc Schmieder
#' @description 
#' @param years vector of years from data
#' @return list of list containing validation indices
getIndicesForOnePrediction <- function(label, size = 0.8) {
  set.seed(100)
  ind <- sample.int(length(label),
                    round((1- size) * length(label)))
  return(list(forOnePrediction = list(train = seq_along(label)[-ind],
                                      val = ind)))
}

#' @author Felix Kleine Bösing
#' @description upsamples data
#' @param data
#' @param label
#' @return list with resampled data and label
upSamplingData <- function(data, label) {
  sampledData = caret::upSample(data, as.factor(label), list = TRUE)
  return(list(data=sampledData$x, 
              label=as.logical(sampledData$y)))
}

#' @author Felix Kleine Bösing
#' @description downsamples data
#' @param data
#' @param label
#' @return list with resampled data and label
downSamplingData <- function(data, label) {
  sampledData = caret::downSample(data, as.factor(label), list = TRUE)
  return(list(data=sampledData$x, 
              label=as.logical(sampledData$y)))
}


#' @author Marc Schmieder
#' @description balance sample data
#' @param data 
#' @param label
#' @param .N  number of observation the sample Dataset should be, If missing then same as input
#' @return list with resampled data and label
balanceSamplingData <- function(data, label, .N, seed = 123){
  set.seed(seed)
  # glueing label and trainData for Rose
  dataSize = length(label)
  freqTable = data.table(label)[, .(count = .N), by = label]
  probs = ifelse(label == 1, (1- freqTable[label == 1,]$count/dataSize),
                 freqTable[label == 1,]$count/dataSize)
  sampleVec = sample.int(as.integer(dataSize), size = .N, prob = probs, replace = T)
  sampledData = data[sampleVec,]
  sampledLabel = as.logical(label[sampleVec])
  
  return(list(data = sampledData, 
              label = sampledLabel))
}

#' @author Felix Kleine Bösing
#' @description downsamples data
#' @param data
#' @return dataframe with encoded data
oneHotEncodeData <- function(data) {
  return(oneHotEncode(data))
}

#' @author Marc Schmieder
#' @description converts logical columns to integer columns
#' @param data
#' @return dataframe with converted data
convertLogicalsToIntegers <- function(data) {
  setDT(data)
  for(name in names(data)){
    if(class(data[, get(name)]) == "logical") {
      data[, c(name) := list(as.integer(get(name)))]
    }
  }
  data = as.data.frame(data)
  return(data)
}



#' @author Felix Kleine Bösing
#' @description downsamples data
#' @param data
#' @return dataframe with encoded data
normalizeData <- function(data) {
  colNames = colnames(data)
  originalStats = list()
  for (i in 1:ncol(data)) {
    vec = data[[i]]
    if (class(vec) %in% c("numeric", "integer", "boolean")) {
      if (!all(unique(vec) %in% c(0, 1))) {
        normalizeInformations = .normalizeVector(vec)
        data[,i] = normalizeInformations$vec
        originalStats[[colNames[i]]] = list(sd = normalizeInformations$sd, mean = normalizeInformations$mean)
      }
    }
  }
  return(data)
}

#' @author Felix Kleine Bösing
#' @description downsamples data
#' @param data
#' @return list: vec= normalized Vector, sd = original standard deviation, mean = original mean 
.normalizeVector <- function(vec) {
  sdVec = sd(vec, na.rm = T)
  meanVec = mean(vec, na.rm = T)
  return(list(vec = (vec - meanVec) / sdVec, sd = sdVec, mean = meanVec))
}


