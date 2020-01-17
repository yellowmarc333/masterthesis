dataPath <- "03_computedData/04_preparedData/"
list.files(dataPath)

#----------------------- 10 percent models ---------------------------------####
# naming is mod_Wordembedding_Algorithm_Subsetsize

# TFIDF 10pc #15-10
fileName <- "TFIDF-10pc-TRUE-FALSE.rds"
mod_TFIDF_XG_10 <- predictXG(dataPath, fileName,
                        sparse = TRUE, nrounds = 70) #0.455

mod_TFIDF_RF_10 <- predictRF(dataPath, fileName, 
                        sparse = TRUE, num.trees = 500) # 0.443 (3k trees tried)

fileName <- "TFIDF-10pc-FALSE-FALSE.rds"
# now tuning
mod_TFIDF_MLP_10 <- predictMLP(dataPath, fileName, epochs = 10) 
# 1: std 0.4946
# 2: with activation = relu everywhere: 0.4703
# 3: 1 with batch norm 48,88
# 4: 1 with dropout 0.3 0.4816
# 5: 1 with learning rate = 0.0001: 0.4791
# 6: 1 with optimizer rmsprop, 0.4924


mod_TFIDF_LogReg_10 <- predictLogReg(dataPath, fileName) # 0.495


# BOW 10pc # 28-11
fileName <- "BOW-10pc-TRUE-FALSE.rds"
mod_BOW_XG_10 <- predictXG(dataPath, fileName,
                           sparse = TRUE, nrounds = 30) # 0.4329
mod_BOW_RF_10 <- predictRF(dataPath, fileName, 
                           sparse = TRUE, num.trees = 10) # 0.449
fileName <- "BOW-10pc-FALSE-FALSE.rds"

mod_BOW_MLP_10 <- predictMLP(dataPath, fileName,
                          epochs = 12) # 0.491
mod_BOW_LogReg_10 <- predictLogReg(dataPath, fileName) # 0.509
mod_BOW_NB_10 <- predictNB(dataPath, fileName)


# CNN Seq 10pc #28-11
fileName <- "Seq-10pc-FALSE.rds"
mod_Seq_CNNSeq_10 <- predictCNNSeq(dataPath, fileName, 
                                   epochs = 12) # 0.4166
# LSTMSeq 10pc #06-10
mod_Seq_LSTMSeq_10 <- predictLSTMSeq(dataPath, fileName, 
                               epochs = 12) # 0.4283

# Seq 10pc on RF and XGB # 28.11
fileName <- "Seq-10pc-FALSE.rds"
mod_Seq_RF_10 <- predictRF(dataPath, fileName, indexName, num.trees = 10) #0.221
mod_Seq_XG_10 <- predictXG(dataPath, fileName, nrounds = 30) #0.219
mod_Seq_MLP_10 <- predictMLP(dataPath, fileName,
                             epochs = 12) # 0.1639

# Sums of word2vec 10PC - 28-11
fileName <- "W2VSums-10pc-50-FALSE.rds"
mod_W2V_NN_10 <- predictMLP(dataPath, fileName) # 0.31
mod_W2V_XG_10 <- predictXG(dataPath, fileName, nrounds = 20) # 0.233
mod_W2V_RF_10 <- predictRF(dataPath, fileName) # 0.26

fileName <- "GloveSums-10pc-300-FALSE.rds"
mod_GloveSum_NN_10 <- predictMLP(dataPath, fileName, epochs = 20) # 0.5693
mod_GloveSum_XG_10 <- predictXG(dataPath, fileName, nrounds = 20) # 0.4747
mod_GloveSum_RF_10 <- predictRF(dataPath, fileName) # 

# Array word2vec 10PC
fileName5 <- "W2VArray-10pc-50-FALSE.rds"
mod_W2VArray_CNNArray_10 <- predictCNNArray(dataPath, fileName5, 
                                            epochs = 12) # 0.3315

# Array Glove 10PC 28-11
fileName <- "GloveArray-10pc-50-FALSE.rds"
mod_GloveArray_CNNArray_10 <- predictCNNArray(dataPath, fileName = fileName, 
                                    epochs = 15) # 0.4913
# Array LSTM 10PC 28-11
mod_GloveArray_LSTMArray_10 <- predictLSTMArray(dataPath, 
                                                fileName = fileName, 
                          epochs = 15) # 0.5126

# Array Glove crawled 10PC 30-11
fileName <- "GloveArray-10pc-300-FALSE.rds"
mod_GloveArray_CNNArray_10 <- predictCNNArray(dataPath, fileName = fileName, 
                                              epochs = 3) 
# 1)tuning: normal 0.5264, 7 am besten mit 0.5355 (epoche 4)
# 2, 1 mit 2x 50 vanilla layer : 0.5212
# 3, 1 ohne vanillas: 0.5330
# 4, 3 mit average pooling: und flatten: 0.5106
# 5, 3 mit filter size 128, 64, 32, 16: 0.5001
# 6, 3 mit filter size umgekehrt 128, 64, 32, 16: 0.5106
# 7, 3 mit 100, 100, 100, 100 0.5317
# 8, 7 mit kernels 2, 3, 4, 5, 0.5355
# 9, 8 + neues layer mit 6 kernels 0.5129



# Array LSTM 10PC 28-11
mod_GloveArray_LSTMArray_10 <- predictLSTMArray(dataPath, 
                                                fileName = fileName, 
                                                epochs = 9) # 0.6011 
# ohne bidirectional 0.5919

# tuning:
# 1: ohne densenet 0.5449
# 2: 1 ohne conv net 0.5701
# 3: 2 mit 256 units 0.5657
# 4: 2 mit nadam 0.5706

# gru: 0.61
#----------------------- 100 percent models --------------------------------####


# BOW 100pc # 13-10
fileName <- "BOW-100pc-TRUE-FALSE.rds"
indexName <- "BOW-Indexes-100pc-TRUE-FALSE.rds"
labelName <- "BOW-Label-100pc-TRUE-FALSE.rds"
mod_BOW_RF_100 <- predictRF(dataPath, fileName, 
                            sparse = TRUE) # 
mod_BOW_XG_100 <- predictXG(dataPath, fileName,
                            sparse = TRUE) # 0.495

# TFIDF 100pc # 13-10
fileName <- "TFIDF-100pc-TRUE-FALSE.rds"
mod_TFIDF_RF_100 <- predictRF(dataPath, fileName,
                              sparse = TRUE, num.trees = 500) # 
mod_TFIDF_XG_100 <- predictXG(dataPath, fileName, 
                              sparse = TRUE, nrounds = 100)  
# 0.590 (nrounds = 100 (mehr wählen))

# LSTM 100pc #06-10
fileName <- "Seq-100pc-FALSE.rds"
indexName <- "Seq-Indexes-100pc-FALSE.rds"
mod_Seq_LSTMSeq_100 <- predictLSTMSeq(dataPath, fileName,
                                epochs = 20) # 0.600

# Seq 100pc # 06-10
fileName7 <- "Seq-100pc-FALSE.rds"
indexName7 <- "Seq-Indexes-100pc-FALSE.rds"
mod_Seq_CNNSeq_100 <- predictCNNSeq(dataPath, fileName7,
                       indexName7, epochs = 15) # 0.580

# Array Glove 100pc # 06-10
fileName <- "GloveArray-100pc-50-FALSE.rds"
indexName <- "Glove-Indexes-100pc-50-FALSE.rds"
mod_GloveArray_CNNArray_100 <- predictCNNArray(dataPath, fileName,
                         indexName) #0.554, 06-10 0.5774

# Array LSTM 100PC 24-10
fileName <- "GloveArray-100pc-50-FALSE.rds"
indexName <- "Glove-Indexes-100pc-50-FALSE.rds"
mod_LSTMArray <- predictLSTMArray(dataPath, fileName = fileName, 
                                  epochs = 20) # 0.5916

# Array LSTM 100PC merge Shortdescription
fileName <- "GloveArray-100pc-50-TRUE.rds"
indexName <- "Glove-Indexes-100pc-50-TRUE.rds"
mod_LSTMGloveTRUE <- predictLSTMArray(dataPath, fileName = fileName, 
                                    indexName = indexName) #0.635

#-------------------------- Full predictions ---------------####

# BOW full # 03.01
fileName <- "BOW-Full-TRUE-FALSE.rds"
mod_BOW_XG_10 <- predictXG(dataPath, fileName,
                           sparse = TRUE, nrounds = 100) # 0.5968
# funktioniert


  #--------------------------- Ensembling ------------------------------------####

accComparison <- data.table(category = names(mod_SeqCNN10$accByClass),
                            SeqCNN10 = mod_SeqCNN10$accByClass,
                            SeqLSTM10 = mod_SeqLSTM10$accByClass,
                            CNNGlove10 =  mod_CNNGlove10$accByClass,
                            LSTMArray10 = mod_LSTMArray10$accByClass, 
                            RF10 = mod_RF10$accByClass)
View(accComparison)
write.rds(accComparison, 
          path = "03_computedData/06_evaluatedData/accComparison.rds",
          compress = 0)

ensSeqeRes1 <- ensSeqleMaxProb(mod_XG2$predictions, 
                               mod_RF2$predictions,
                               mod_Seq$predictions,
                               truth = mod_XG2$testLabelRaw)

# saving
dateNow <- gsub(date(), pattern = " ",
                replacement = "_")
dateNow <- gsub(dateNow, pattern = ":",
                replacement = "-")

for(file in grep(x = ls(), pattern = "mod_", value = TRUE)){
  saveRDS(get(file, inherits = FALSE),
          file = paste0("03_computedData/05_modelData/",
                                   file,
                        dateNow, ".RDS"))
}

