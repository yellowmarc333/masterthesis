dataPath <- "03_computedData/04_preparedData/"
list.files(dataPath)

#----------------------- 10 percent models ---------------------------------####
# naming is mod_Seqedding_Algorithm_Subsetsize

# LSTM 10pc #06-10
fileName <- "Seq-10pc-FALSE.rds"
mod_Seq_LSTM_10 <- predictLSTM(dataPath, fileName, 
                        epochs = 10) # 0.437

# Seq 10pc #06-10
mod_Seq_CNN_10 <- predictSeq(dataPath, fileName, 
                        epochs = 10) # 0.426

# Seq 10pc on RF and XGB #02.08
fileName <- "Seq-10pc-FALSE.rds"
indexName <- "Seq-Indexes-10pc-FALSE.rds"
mod_Seq_RF_10 <- predictRF(dataPath, fileName, 
                            indexName) #0.221
mod_Seq_XG_10 <- predictXG(dataPath, fileName, 
                           nrounds = 30) #0.218

# TFIDF 10pc #15-10
fileName <- "TFIDF-10pc-TRUE-FALSE.rds"
mod_TFIDF_XG_10 <- predictXG(dataPath, fileName,
                        sparse = TRUE,
                        nrounds = 30) 
# mit neuen weights: 0.423
mod_TFIDF_RF_10 <- predictRF(dataPath, fileName, 
                        sparse = TRUE)
# mit neuen weights: 0.44358

fileName <- "TFIDF-10pc-FALSE-FALSE.rds"
mod_TFIDF_MLP_10 <- predictMLP(dataPath, fileName, epochs = 10) # 0.500

mod_TFIDF_LogReg_10 <- predictLogReg(dataPath, fileName) # 0.495
# mit neuen weights:

# BOW 10pc # 15-10
fileName <- "BOW-10pc-TRUE-FALSE.rds"
indexName <- "BOW-Indexes-10pc-TRUE-FALSE.rds"
labelName <- "BOW-Label-10pc-TRUE-FALSE.rds"
mod_BOW_XG_10 <- predictXG(dataPath, fileName,
                           sparse = TRUE, nrounds = 30) # 0.4344
mod_BOW_RF_10 <- predictRF(dataPath, fileName, 
                           sparse = TRUE, num.trees = 500) # 0.455
fileName <- "BOW-10pc-FALSE-FALSE.rds"
indexName <- "BOW-Indexes-10pc-FALSE-FALSE.rds"
mod_BOW_MLP_10 <- predictMLP(dataPath, fileName,
                          epochs = 12) # 0.491
mod_BOW_LogReg_10 <- predictLogReg(dataPath, fileName) # 0.509
mod_BOW_NB_10 <- predictNB(dataPath, fileName)

# Sums of word2vec 10PC
fileName3 <- "W2V-10pc-50-FALSE.rds"
mod_W2V_NN_10 <- predictMLP(dataPath, fileName3) # 
mod_W2V_XG_10 <- predictXG(dataPath, fileName3) # 0.14
mod_W2V_RF_10 <- predictRF(dataPath, fileName3) # 0.17

# Array word2vec 10PC
fileName5 <- "W2VArray-10pc-50-FALSE.rds"
mod_W2VArray_CNN_10 <- predictCNN(dataPath, fileName5) # 0.15

# Array Glove 10PC 24-10
fileName <- "GloveArray-10pc-50-FALSE.rds"
indexName <- "Glove-Indexes-10pc-50-FALSE.rds"
mod_GloveArray_CNN_10 <- predictCNN(dataPath, fileName = fileName, 
                                    epochs = 12) # 0.484
# Array LSTM 10PC 24-10
mod_GloveArray_LSTMArray_10 <- predictLSTMArray(dataPath, 
                                                fileName = fileName, 
                          epochs = 15) # 0.5119



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
indexName <- "TFIDF-Indexes-100pc-TRUE-FALSE.rds"
labelName <- "TFIDF-Label-100pc-TRUE-FALSE.rds"
mod_TFIDF_RF_100 <- predictRF(dataPath, fileName,
                              sparse = TRUE) # 
mod_TFIDF_XG_100 <- predictXG(dataPath, fileName, 
                              sparse = TRUE) # 


# LSTM 100pc #06-10
fileName <- "Seq-100pc-FALSE.rds"
indexName <- "Seq-Indexes-100pc-FALSE.rds"
mod_Seq_LSTM_100 <- predictLSTM(dataPath, fileName,
                                epochs = 20) # 0.600

# Seq 100pc # 06-10
fileName7 <- "Seq-100pc-FALSE.rds"
indexName7 <- "Seq-Indexes-100pc-FALSE.rds"
mod_Seq_CNN_100 <- predictSeq(dataPath, fileName7,
                       indexName7, epochs = 15) # 0.580

# Array Glove 100pc # 06-10
fileName <- "GloveArray-100pc-50-FALSE.rds"
indexName <- "Glove-Indexes-100pc-50-FALSE.rds"
mod_GloveArray_CNN_100 <- predictCNN(dataPath, fileName,
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

