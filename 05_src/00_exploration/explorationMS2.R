# to do: set up h2o to work
# rename column names from data in preparation
# add error measurement of the network


## set up h2o
# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
pkgs <- c("RCurl","jsonlite")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", 
                 repos="http://h2o-release.s3.amazonaws.com/h2o/rel-yates/5/R")


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
                                  data = dataNnet, hidden = c(3))
# saving the model
saveRDS(modelNnet, file = "03_computedData/05_modelData/modelNnet.rds")

resultResponse <- data.table(modelNnet$response)
resultProbs <- data.table(modelNnet$net.result[[1]])

resultDiffProb <- abs(resultResponse - resultProbs)    
errorProbDiff <- sum(resultDiffProb)/prod(dim(resultDiffProb))
# 0.03408962 with training data only

setnames(resultProbs, colnames(resultProbs), colnames(resultResponse))

meanProbError <- mean(as.vector(abs(resultResponse - resultProbs)))
# now see if the probabilities and the true false values differ 
# 


# Finally, let's load H2O and start up an H2O cluster
library(h2o)
h2o.init()

h2o.init(nthreads=-1, max_mem_size="2G")
h2o.removeAll()
h2o.clusterInfo()

args(h2o.deeplearning)
help(h2o.deeplearning)
example(h2o.deeplearning)
demo(h2o.deeplearning)  #requires user interaction


data.h2o <- as.h2o(data)
model.h2o <- h2o.deeplearning(y = "labelRaw", training_frame = data.h2o)
predict.h2o <- h2o.predict(object = model.h2o, newdata = data.h2o)




# next steps:
# 3blue one brown deep learning serie anschauen + notizen
# liste machen mit methoden die ausprobiert werden können
# random forest, xgboost, multinomial naive bayes
# recurrent neural net, hierarchical attention neural net
# nach ähnlichen Datensätzen suchen
# markus seine methode im blick behalten