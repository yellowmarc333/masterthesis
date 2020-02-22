# testsubsets ####
# checks the predictions for all models and identifies the data points
# for which all are correct or none are correct
# it returns the indizes of the indiviual embeddings for later processing
res <- identifyTestSubsets(inPath = "03_computedData/05_modelData/finalModelsBackup/",
                    embeddingPath = "03_computedData/04_preparedData/")

saveRDS(res, "03_computedData/06_evaluatedData/testSubsets.RDS")

# analyse individual models to changes ####
try(resXG <- explainXGBoost(modelPath = "03_computedData/05_modelData/OnlyModelSave/xgb.RDS",
               embeddingPath = "03_computedData/04_preparedData/BOW-Full-TRUE-FALSE.rds"))
try(saveRDS(resXG, "03_computedData/06_evaluatedData/resXG.RDS"))


try(resMLP <- explainMLP(modelPath = "03_computedData/05_modelData/OnlyModelSave/mod_GloveSumsFull300_MLP.h5",
                        embeddingPath = "03_computedData/04_preparedData/GloveArray-Full-300-FALSE.rds"))
try(saveRDS(resMLP, "03_computedData/06_evaluatedData/resMLP.RDS"))


resCNN <- explainCNN(modelPath = "03_computedData/05_modelData/OnlyModelSave/mod_GloveArrayFull300_CNN.h5",
           embeddingPath = "03_computedData/04_preparedData/GloveArray-Full-300-FALSE.rds")

saveRDS(resCNN, "03_computedData/06_evaluatedData/resCNN.RDS")
#resCNN <- readRDS("03_computedData/06_evaluatedData/resCNN.RDS")

resLSTM <- explainLSTM(modelPath = "03_computedData/05_modelData/OnlyModelSave/mod_GloveArrayFull300_LSTM.h5",
            embeddingPath = "03_computedData/04_preparedData/GloveArray-Full-300-FALSE.rds")
saveRDS(resLSTM, "03_computedData/06_evaluatedData/resLSTM.RDS")

#resLSTM <- readRDS("03_computedData/06_evaluatedData/resLSTM.RDS")

# individual datapoints explaining ####
res <- explainIndividual(modelPath = "03_computedData/05_modelData/OnlyModelSave/mod_GloveArrayFull300_CNN.h5",
                         embeddingPath = "03_computedData/04_preparedData/")
saveRDS(res, "03_computedData/06_evaluatedData/explainIndividual.RDS")
res <- readRDS("03_computedData/06_evaluatedData/explainIndividual.RDS")


# ggPlot der einzelnen Wahrscheinlichkeiten der Modelle ####
ggDataPoint1 <- plotIndividualProbs(explainData = res, index = 1)
ggDataPoint1

# ggPlot Verlauf Seq LSTM
# explainData ist teile der liste von explainIndividual
ggObj <- plotLSTMSeq(explainData = res[[3]], index = 1)

## xgb ####
test <- xgb.importance(model = xgBModel)
View(test)
test2 <- xgb.model.dt.tree(model = xgBModel)
View(test2)
length(unique(test2$Tree))





