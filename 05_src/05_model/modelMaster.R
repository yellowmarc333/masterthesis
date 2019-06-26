
# --------H2o--------------------------------
library(h2o)
h2o.init()

h2o.init(nthreads=-1, max_mem_size="2G")
h2o.removeAll()

args(h2o.deeplearning)
help(h2o.deeplearning)
example(h2o.deeplearning)
demo(h2o.deeplearning)  #requires user interaction

data <- read.fst("03_computedData/04_preparedData/NewsWithLabel0.01Subset.fst",
                 as.data.table = TRUE)
indexes <- sample.int(nrow(data), size = round(nrow(data) * 0.75))
trainData <- data[indexes]
testData <- data[-indexes]
trainData.h2o <- as.h2o(trainData)
testData.h2o <- as.h2o(testData)
model.h2o <- h2o.deeplearning(y = "labelRaw", training_frame = trainData.h2o, 
                              model_id = "1")
# modelLocal <- h2o.getModel(model_id = "1")

predict.h2o <- h2o.predict(object = model.h2o, newdata = testData.h2o)
predict.local <- as.data.table(predict.h2o)
confMat <- h2o.confusionMatrix(model.h2o)


# ----------neuralnet -----------------------------------------

data <- read.fst("03_computedData/04_preparedData/NewsWithLabel0.01Subset.fst",
                 as.data.table = TRUE)
newNames <- colnames(data)
for (symbol in 100:1){
  newNames <- gsub(newNames, pattern = symbol, replacement = paste0("V", symbol)
                   , fixed = TRUE)
}
setnames(data, colnames(data), newNames)
# try first neural net, nnet cant handle too many weights
# try neuralnet now

target <- "labelRaw"
dependentCols <- newNames[!(newNames %in% target)]

# trainData <- data[, .SD, .SDcols = dependentCols]
# trainLabel <- data[[target]]

labelEncoded <- oneHotEncode(data$labelRaw)
labelNames <- names(labelEncoded)
newLabelNames <- gsub(labelNames, pattern = "&", replacement = "_")
newLabelNames <- gsub(newLabelNames, pattern = " ", replacement = "")
setnames(labelEncoded, labelNames, newLabelNames)

dataNnet <- data.table(labelEncoded, data[, .SD, .SDcols = dependentCols])

f <- as.formula(paste(paste(newLabelNames, collapse = "+"),
                      " ~ ", 
                      paste(dependentCols ,
                            collapse= "+")))
modelNnet <- neuralnet::neuralnet(formula = f,
                                  data = dataNnet, hidden = c(100))
# saving the model
saveRDS(modelNnet, file = "03_computedData/05_modelData/modelNnet.rds")

resultResponse <- data.table(modelNnet$response)
resultProbs <- data.table(modelNnet$net.result[[1]])

resultDiffProb <- abs(resultResponse - resultProbs)    
errorProbDiff <- sum(resultDiffProb)/prod(dim(resultDiffProb))
# 0.03408962 with training data only, hidden = c(3)
