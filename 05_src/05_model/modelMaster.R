dataPath <- "03_computedData/04_preparedData/"
list.files(dataPath)

#----------------------- 10 percent models ---------------------------------####

# LSTM 10pc #26-07
fileName <- "Emb-10pc-FALSE.fst"
indexName <- "Emb-Indexes-10pc-FALSE.fst"
resultEmbLSTM10 <- predictLSTM(dataPath, fileName, 
                        indexName, subsetSize = "10pc") # 0.424

# Emb 10pc #15-07
fileName <- "Emb-10pc-FALSE.fst"
indexName <- "Emb-Indexes-10pc-FALSE.fst"
resultEmbCNN10 <- predictEmb(dataPath, fileName, 
                        indexName, subsetSize = "10pc") # 0.412

# TFIDF 10pc #15-07
fileName1 <- "TFIDF-10pc--FALSE.fst" 
resultXG1 <- predictXG(dataPath, fileName1, subsetSize = "10pc") # 
resultRF1 <- predictRF(dataPath, fileName1, subsetSize = "10pc") # 


# BOW 10pc # 15-07
fileName <- "BOW-10pc-FALSE.fst" 
indexName <- "BOW-Indexes-10pc-FALSE.fst"
resultXG10 <- predictXG(dataPath, fileName,
                       indexName, subsetSize = "10pc") # 0.399
resultRF10 <- predictRF(dataPath, fileName,
                       indexName, subsetSize = "10pc") # 0.443


# Sums of word2vec 10PC
fileName3 <- "W2V-10pc-50-FALSE.fst"
resultNN3 <- predictNN(dataPath, fileName3) # 
resultXG3 <- predictXG(dataPath, fileName3, subsetSize = "10pc") # 0.14
resultRF3 <- predictRF(dataPath, fileName3, subsetSize = "10pc") # 0.17

# Array word2vec 10PC
fileName5 <- "W2VArray-10pc-50-FALSE.rds"
resultCN <- predictCNN(dataPath, fileName5, subsetSize = "10pc") # 0.15

# Array Glove 10PC
fileName <- "GloveArray-10pc-50-FALSE.rds"
indexName <- "Glove-Indexes-10pc-50-FALSE.fst"
resultCNNGlove10 <- predictCNN(dataPath, fileName = fileName, 
                           indexName = indexName, subsetSize = "10pc") # 0.47
# Array LSTM 10PC
fileName <- "GloveArray-10pc-50-FALSE.rds"
indexName <- "Glove-Indexes-10pc-50-FALSE.fst"
resultLSTMArray10 <- predictLSTMArray(dataPath, fileName = fileName, 
                           indexName = indexName, subsetSize = "10pc") # 0.496

#----------------------- 100 percent models --------------------------------####

# BOW 100pc # 26-07
fileName <- "BOW-100pc-FALSE.fst" 
indexName <- "BOW-Indexes-100pc-FALSE.fst"
resultXG100 <- predictXG(dataPath, fileName,
                       indexName, subsetSize = "100pc") # 
saveRDS(resultXG100,
        file = paste0("03_computedData/05_modelData/",
                      "resultXG100.RDS"))
resultRF100 <- predictRF(dataPath, fileName,
                       indexName, subsetSize = "100pc") # 
saveRDS(resultRF100,
        file = paste0("03_computedData/05_modelData/",
                      "resultRF100.RDS"))


# Sums of word2vec 100PC
fileName4 <- "W2V-100pc-50-FALSE.fst"
resultXG4 <- predictXG(dataPath, fileName4, subsetSize = "100pc") # 18.6
resultRF4 <- predictRF(dataPath, fileName4, subsetSize = "100pc") # 

# Array word2vec 100PC
fileName6 <- "W2VArray-100pc-50-FALSE.rds"
resultCN <- predictCNN(dataPath, fileName6, subsetSize = "100pc") # 0.208

# LSTM 100pc #26-07
fileName <- "Emb-100pc-FALSE.fst"
indexName <- "Emb-Indexes-100pc-FALSE.fst"
resultLSTM2 <- predictLSTM(dataPath, fileName, 
                         indexName, subsetSize = "100pc") # 0.5746

# Emb 100pc # 15-07
fileName7 <- "Emb-100pc-FALSE.fst"
indexName7 <- "Emb-Indexes-100pc-FALSE.fst"
resultEmb7 <- predictEmb(dataPath, fileName7,
                       indexName7, subsetSize = "10pc") # 0.551

# Array Glove 100pc # 15-07
fileName <- "GloveArray-100pc-50-FALSE.rds"
indexName <- "Glove-Indexes-100pc-50-FALSE.fst"
resultEmb2 <- predictCNN(dataPath, fileName,
                         indexName, subsetSize = "100pc") #0.554

# Array LSTM 100PC
fileName <- "GloveArray-100pc-50-FALSE.rds"
indexName <- "Glove-Indexes-100pc-50-FALSE.fst"
resultLSTMArray <- predictLSTMArray(dataPath, fileName = fileName, 
                                    indexName = indexName,
                                    subsetSize = "100pc") # 0.579

#--------------------------- Ensembling ------------------------------------####

accComparison <- data.table(cbind(resultEmbCNN10$accByClass,
                                  resultEmbLSTM10$accByClass,
                                  resultCNNGlove10$accByClass,
                                  resultLSTMArray10$accByClass, 
                                  resultRF10$accByClass))
View(accComparison)

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

