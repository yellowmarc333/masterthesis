#' Down-, up- and balanced-Sampling for imbalanced Data with binary classes
#'
#' This function combines commonly used sampling methods. The user might choose
#' a simple up- or downsampling or a fusion of those two methods with
#' determining the size of the resulting data.table as well as the probability
#' of the minority class. Additionally, it is possible to input data and label
#' vector seperately, or input the entire data with the label name as a
#' character. The return value corresponds to the input values.
#'
#' @author Marc Schmieder
#' @param data \code{data.table}. Input data.
#' @param label \code{logical}, \code{integer} or \code{character(1)}.
#'        Either a binary label vector or the name of the label.
#' @param method \code{character(1)}. Sampling method. Choose out of
#'        "up", "down", "balance" or "none".
#' @param N \code{numeric(1)}. Size ratio of the resulting data
#'         in relation to the input data. Default is 1.
#' @param p \code{numeric(1)} in (0, 1). Probability of the minority class.
#'         Default is 0.5.
#' @return Depends on the input. If the input is data and label
#'         seperate, the value is a \code{list(2)} with entries data and label.
#'         If label is \code{character(1)}, the function returns
#'         a \code{data.table}.
#' @examples
#' x <- as.data.table(Titanic)
#' label <- c(rep(FALSE, nrow(x) - 3), rep(TRUE, 3))
#' x[, Survived := NULL]
#' generalizedSampling(x, label, method = "balance", N = 0.2)
#'
#' x <- as.data.table(Titanic)
#' res <- generalizedSampling(x, "Survived", N = 1,
#'                            p = 0.3, method = "balance")
#' table(res$Survived)
#' @export
generalizedSampling <- function(data, label,
                                method = c("up", "down", "balance", "none"),
                                N = 1, p = 0.5){
  assertDataTable(data)
  assert(checkAtomicVector(label, len = nrow(data)),
         checkCharacter(label), combine = "or")
  method <- match.arg(method)
  assertNumber(N)
  assertNumber(p, lower = 0, upper = 1)
  data <- copy(data)
  
  if (testString(label)) {
    labelName <- label
    label <- data[[label]]
    returnMethod <- "data.table"
  } else {
    returnMethod <- "list"
  }
  # this case is for framework use, where depending on the balanced method
  # always the same return value is expected
  if (method == "none") {
    ifelse (returnMethod == "list",
           return(list(data = data,
                       label = label)),
           return(data))
  }

  
  dataSize <- length(label)

  freqTable <- data.table(label)[, .(count = .N), by = label]
  setorderv(freqTable, "count", -1L)
  
  # multilabel case
  if (length(unique(label)) > 2) {
    if (method == "up") multiN <- max(freqTable$count)
    if (method == "down") multiN <- min(freqTable$count)
    
    if(returnMethod == "list") data[, label := label]
    
    tmpDataList <- list()
    for (index in seq_len(nrow(freqTable))) {
      category <- freqTable$label[index]
      categorySize <- freqTable$count[index]
      
      if(categorySize < multiN) tmpIndizes <- sample.int(categorySize, 
                                                         size = multiN,
                                                         replace = TRUE)
      if(categorySize >= multiN) tmpIndizes <- sample.int(categorySize, 
                                                          size = multiN,
                                                          replace = FALSE)
      # todo labelRaw variabel ersetzen
      tmpData <- data[labelRaw == category,][tmpIndizes]
      
      tmpDataList <- c(tmpDataList, list(tmpData))
    }
    sampledData <- rbindlist(tmpDataList)
    
    
    if (returnMethod == "list") {
      sampledLabel <- sampledData$label
      sampledData[, label := NULL]
      return(list(data = sampledData,
                  label = sampledLabel))
    }
    
    if (returnMethod == "data.table") {
      return(sampledData)
    }
  }
  
  
  classesOrdered <- freqTable[, label]
  classesFreq <- freqTable[, count / sum(count)]
  classesInvFreq <- (1 - classesFreq) / sum(1 - classesFreq)

  minorityIndex <- which(label == last(classesOrdered))
  majorityIndex <- which(label == first(classesOrdered))
  minorityN <- ceiling(p * N * dataSize)
  majorityN <- floor((1 - p) * N * dataSize)

  sampleVec <- switch(method,
                      balance = c(sample(minorityIndex,
                                       minorityN, replace = TRUE),
                                  sample(majorityIndex,
                                         majorityN, replace = TRUE)),
                      up = c(sample(minorityIndex, first(freqTable$count) -
                                      last(freqTable$count),
                                    replace = TRUE),
                             majorityIndex, minorityIndex),
                      down = c(sample(majorityIndex, last(freqTable$count),
                                      replace = FALSE),
                               minorityIndex))

  sampledData <- data[sampleVec, ]
  sampledLabel <- label[sampleVec]

  if (returnMethod == "list") {
    return(list(data = sampledData,
                label = sampledLabel))
  }
  if (returnMethod == "data.table") {
    sampledData[, (labelName) := sampledLabel]
    return(sampledData)
  }
}

# TODO
# testing multilabel with different return methods
# implementing choosing of N
