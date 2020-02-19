# checks the predictions for all models and identifies the data points
# for which all are correct or none are correct
# it returns the indizes of the indiviual embeddings for later processing
res <- identifyTestSubsets(inPath = "03_computedData/05_modelData/finalModelsBackup/",
                    embeddingPath = "03_computedData/04_preparedData/")

saveRDS(res, "03_computedData/06_evaluatedData/testSubsets.RDS")


explainXGBoost(inPath = "03_computedData/05_modelData/finalModels/", 
               modelName = "mod_BOW_XG_Full_2020-01-29.RDS",
               embeddingPath = "03_computedData/04_preparedData/BOW-Full-TRUE-FALSE.rds")


resCNN <- explainCNN(modelPath = "03_computedData/05_modelData/OnlyModelSave/mod_GloveArrayFull300_CNN.h5",
           embeddingPath = "03_computedData/04_preparedData/GloveArray-Full-300-FALSE.rds")

saveRDS(resCNN, "03_computedData/06_evaluatedData/resCNN.RDS")
#resCNN <- readRDS("03_computedData/06_evaluatedData/resCNN.RDS")

resLSTM <- explainLSTM(modelPath = "03_computedData/05_modelData/OnlyModelSave/mod_GloveArrayFull300_LSTM.h5",
            embeddingPath = "03_computedData/04_preparedData/GloveArray-Full-300-FALSE.rds")
saveRDS(resLSTM, "03_computedData/06_evaluatedData/resLSTM.RDS")

#resLSTM <- readRDS("03_computedData/06_evaluatedData/resLSTM.RDS")















