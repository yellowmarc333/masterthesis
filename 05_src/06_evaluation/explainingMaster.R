# global parameters
fullWidth <- 15.49779
fullHeight <- fullWidth * 9/16
fullWidth2 <- 15.49779 * 3/4
fullHeight2 <- fullWidth * 9/29
outPath <- "03_computedData/06_evaluatedData/Individual/"

# testsubsets ####
# checks the predictions for all models and identifies the data points
# for which all are correct or none are correct
# it returns the indizes of the indiviual embeddings for later processing
res <- identifyTestSubsets(inPath = "03_computedData/05_modelData/finalModelsBackup/",
                    embeddingPath = "03_computedData/04_preparedData/")
saveRDS(res, "03_computedData/06_evaluatedData/testSubsets.RDS")
res <- readRDS("03_computedData/06_evaluatedData/testSubsets.RDS")
# wie viele Beobachtungen werden vom LSTM richtig erkannt und von anderen nicht?
res$res[GloveArray300_LSTMArray == TestLabel &
          GloveArray300_CNNArray != TestLabel &
          GloveSums300_MLP != TestLabel &
          BOW_XG != TestLabel, .N]   # 547

# analyse individual models to changes ####
try(resXG <- explainXGBoost(modelPath = "03_computedData/05_modelData/OnlyModelSave/xgb.RDS",
               embeddingPath = "03_computedData/04_preparedData/BOW-Full-TRUE-FALSE.rds"))
try(saveRDS(resXG, "03_computedData/06_evaluatedData/resXG.RDS"))
resXG <- readRDS("03_computedData/06_evaluatedData/resXG.RDS")

try(resMLP <- explainMLP(modelPath = "03_computedData/05_modelData/OnlyModelSave/mod_GloveSumsFull300_MLP.h5",
                        embeddingPath = "03_computedData/04_preparedData/GloveArray-Full-300-FALSE.rds"))
try(saveRDS(resMLP, "03_computedData/06_evaluatedData/resMLP.RDS"))
resMLP <- readRDS("03_computedData/06_evaluatedData/resMLP.RDS")

resCNN <- explainCNN(modelPath = "03_computedData/05_modelData/OnlyModelSave/mod_GloveArrayFull300_CNN.h5",
           embeddingPath = "03_computedData/04_preparedData/GloveArray-Full-300-FALSE.rds")

saveRDS(resCNN, "03_computedData/06_evaluatedData/resCNN.RDS")
resCNN <- readRDS("03_computedData/06_evaluatedData/resCNN.RDS")

resLSTM <- explainLSTM(modelPath = "03_computedData/05_modelData/OnlyModelSave/mod_GloveArrayFull300_LSTM.h5",
            embeddingPath = "03_computedData/04_preparedData/GloveArray-Full-300-FALSE.rds")
saveRDS(resLSTM, "03_computedData/06_evaluatedData/resLSTM.RDS")
resLSTM <- readRDS("03_computedData/06_evaluatedData/resLSTM.RDS")

# plot for modifySequence
ggObj <- plotSeqSimulation(resXG, resCNN, resLSTM, resMLP)
ggsave(filename = "03_computedData/06_evaluatedData/plotSeqSimulation.pdf",
       plot = ggObj, width = fullWidth, height = fullHeight, 
       device = "pdf")

# individual datapoints explaining ####
res <- explainIndividual(modelPath = "03_computedData/05_modelData/OnlyModelSave/mod_GloveArrayFull300_CNN.h5",
                         embeddingPath = "03_computedData/04_preparedData/")
saveRDS(res, "03_computedData/06_evaluatedData/explainIndividual.RDS")
res <- readRDS("03_computedData/06_evaluatedData/explainIndividual.RDS")


# ggPlots der einzelnen Wahrscheinlichkeiten der Modelle ####
res <- readRDS("03_computedData/06_evaluatedData/explainIndividual.RDS")

outPath <- "03_computedData/06_evaluatedData/IndividualPrettier/"

for(i in c(13)) {
  i <- 13
  ggIndProbs <- plotIndividualProbs(explainData = res, index = i)
  ggsave(filename = paste0(outPath, "ggIndProbs", i, ".pdf"),
         plot = ggIndProbs, width = fullWidth, height = fullHeight, 
         device = "pdf")
  
  # explainData ist teile der liste von explainIndividual
  plotDataXG <- calcPD_XG(explainData = res[[1]], index = i,
                          ModelName = "XGBoost",
                          modelPath = "03_computedData/05_modelData/OnlyModelSave/xgb.RDS")
  
  plotDataCNN <- calcPD_LSTMCNN(explainData = res[[2]], index = i,
                                ModelName = "CNN",
                                modelPath = "03_computedData/05_modelData/OnlyModelSave/mod_GloveArrayFull300_CNN.h5")
  
  plotDataLSTM <- calcPD_LSTMCNN(explainData = res[[3]], index = i,
                                 ModelName = "Bi-LSTM",
                                 modelPath = "03_computedData/05_modelData/OnlyModelSave/mod_GloveArrayFull300_LSTM.h5")
  
  plotDataMLP <- calcPD_MLP(explainData = res[[4]], index = i,
                            ModelName = "MLP",
                            modelPath = "03_computedData/05_modelData/OnlyModelSave/mod_GloveSumsFull300_MLP.h5")
  
  ggObj <- plotSeqInd(plotDataXG, plotDataLSTM, plotDataCNN, plotDataMLP)
  ggsave(filename = paste0(outPath, "plotSeqInd", i, ".pdf"),
         plot = ggObj, width = fullHeight*1.1, height = fullWidth*0.9, 
         device = "pdf")
}




## xgb ####
test <- xgb.importance(model = xgBModel)
View(test)
test2 <- xgb.model.dt.tree(model = xgBModel)
View(test2)
length(unique(test2$Tree))





