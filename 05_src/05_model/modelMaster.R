dataPath <- "03_computedData/04_preparedData/"
list.files(dataPath)

#----------------------- 10 percent models ---------------------------------####

# LSTM 10pc #06-10
fileName <- "Emb-10pc-FALSE.fst"
indexName <- "Emb-Indexes-10pc-FALSE.fst"
resultEmbLSTM10 <- predictLSTM(dataPath, fileName, 
                        indexName, epochs = 8) # 0.424

# Emb 10pc #06-10
fileName <- "Emb-10pc-FALSE.fst"
indexName <- "Emb-Indexes-10pc-FALSE.fst"
resultEmbCNN10 <- predictEmb(dataPath, fileName, 
                        indexName, epochs = 8) # 0.412

# Emb 10pc on RF and XGB #02.08
fileName <- "Emb-10pc-FALSE.fst"
indexName <- "Emb-Indexes-10pc-FALSE.fst"
resultEmbRF10 <- predictRF(dataPath, fileName, 
                             indexName) #0.221
resultEmbXG10 <- predictXG(dataPath, fileName, 
                           indexName) #0.218

# TFIDF 10pc #15-07
fileName1 <- "TFIDF-10pc--FALSE.fst" 
resultXG1 <- predictXG(dataPath, fileName1) # 
resultRF1 <- predictRF(dataPath, fileName1) # 


# BOW 10pc # 15-07
fileName <- "BOW-10pc-FALSE.fst" 
indexName <- "BOW-Indexes-10pc-FALSE.fst"
resultXG10 <- predictXG(dataPath, fileName,
                       indexName) # 0.399
resultRF10 <- predictRF(dataPath, fileName,
                       indexName) # 0.443


# Sums of word2vec 10PC
fileName3 <- "W2V-10pc-50-FALSE.fst"
resultNN3 <- predictNN(dataPath, fileName3) # 
resultXG3 <- predictXG(dataPath, fileName3) # 0.14
resultRF3 <- predictRF(dataPath, fileName3) # 0.17

# Array word2vec 10PC
fileName5 <- "W2VArray-10pc-50-FALSE.rds"
resultCN <- predictCNN(dataPath, fileName5) # 0.15

# Array Glove 10PC
fileName <- "GloveArray-10pc-50-FALSE.rds"
indexName <- "Glove-Indexes-10pc-50-FALSE.fst"
resultCNNGlove10 <- predictCNN(dataPath, fileName = fileName, 
                           indexName = indexName, epochs = 10) # 0.47, less categories: 0.49
# Array LSTM 10PC
fileName <- "GloveArray-10pc-50-FALSE.rds"
indexName <- "Glove-Indexes-10pc-50-FALSE.fst"
resultLSTMArray10 <- predictLSTMArray(dataPath, fileName = fileName, 
                           indexName = indexName, epochs = 10) # 0.496, less categories: 0.523

# EmbLSTM 10pc shortdescription #03-08
fileName <- "Emb-10pc-TRUE.fst"
indexName <- "Emb-Indexes-10pc-TRUE.fst"
resultEmbLSTM10SD <- predictLSTM(dataPath, fileName, 
                               indexName) # 0.4124

#----------------------- 100 percent models --------------------------------####

# BOW 100pc # 26-07
fileName <- "BOW-100pc-FALSE.fst" 
indexName <- "BOW-Indexes-100pc-FALSE.fst"
resultXG100 <- predictXG(dataPath, fileName,
                       indexName) # 
saveRDS(resultXG100,
        file = paste0("03_computedData/05_modelData/",
                      "resultXG100.RDS"))
resultRF100 <- predictRF(dataPath, fileName,
                       indexName) # 
saveRDS(resultRF100,
        file = paste0("03_computedData/05_modelData/",
                      "resultRF100.RDS"))


# Sums of word2vec 100PC
fileName4 <- "W2V-100pc-50-FALSE.fst"
resultXG4 <- predictXG(dataPath, fileName4) # 18.6
resultRF4 <- predictRF(dataPath, fileName4) # 

# Array word2vec 100PC
fileName6 <- "W2VArray-100pc-50-FALSE.rds"
resultCN <- predictCNN(dataPath, fileName6) # 0.208

# LSTM 100pc #06-10
fileName <- "Emb-100pc-FALSE.fst"
indexName <- "Emb-Indexes-100pc-FALSE.fst"
resultLSTM2 <- predictLSTM(dataPath, fileName, 
                         indexName, epochs = 20) # 0.600

# Emb 100pc # 06-10
fileName7 <- "Emb-100pc-FALSE.fst"
indexName7 <- "Emb-Indexes-100pc-FALSE.fst"
resultEmb7 <- predictEmb(dataPath, fileName7,
                       indexName7, epochs = 15) # 0.580

# Array Glove 100pc # 06-10
fileName <- "GloveArray-100pc-50-FALSE.rds"
indexName <- "Glove-Indexes-100pc-50-FALSE.fst"
resultEmb2 <- predictCNN(dataPath, fileName,
                         indexName) #0.554, 06-10 0.5774

# Array LSTM 100PC
fileName <- "GloveArray-100pc-50-FALSE.rds"
indexName <- "Glove-Indexes-100pc-50-FALSE.fst"
resultLSTMArray <- predictLSTMArray(dataPath, fileName = fileName, 
                                    indexName = indexName) # 0.5712

# Array LSTM 100PC merge Shortdescription
fileName <- "GloveArray-100pc-50-TRUE.rds"
indexName <- "Glove-Indexes-100pc-50-TRUE.fst"
resultLSTMGloveTRUE <- predictLSTMArray(dataPath, fileName = fileName, 
                                    indexName = indexName) #0.635

#--------------------------- Ensembling ------------------------------------####

accComparison <- data.table(category = names(resultEmbCNN10$accByClass),
                            EmbCNN10 = resultEmbCNN10$accByClass,
                            EmbLSTM10 = resultEmbLSTM10$accByClass,
                            CNNGlove10 =  resultCNNGlove10$accByClass,
                            LSTMArray10 = resultLSTMArray10$accByClass, 
                            RF10 = resultRF10$accByClass)
View(accComparison)
write.fst(accComparison, 
          path = "03_computedData/06_evaluatedData/accComparison.fst",
          compress = 0)

ensembeRes1 <- ensembleMaxProb(resultXG2$predictions, 
                               resultRF2$predictions,
                               resultEmb$predictions,
                               truth = resultXG2$testLabelRaw)


dateNow <- gsub(date(), pattern = " ",
                replacement = "_")
dateNow <- gsub(dateNow, pattern = ":",
                replacement = "-")

for(file in grep(x = ls(), pattern = "result", value = TRUE)){
  saveRDS(get(file, inherits = FALSE),
          file = paste0("03_computedData/05_modelData/",
                                   file,
                        dateNow, ".RDS"))
}

