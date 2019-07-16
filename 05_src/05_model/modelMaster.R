dataPath <- "03_computedData/04_preparedData/"
list.files(dataPath)


# Emb 10pc #15-07
fileName8 <- "Emb-10pc-FALSE.fst"
indexName8 <- "Emb-Indexes-10pc-FALSE.fst"
resultEmb <- predictEmb(dataPath, fileName8, 
                        indexName8, subsetSize = "10pc") # 0.412

# TFIDF 10pc #15-07
fileName1 <- "TFIDF-10pc--FALSE.fst" 
resultXG1 <- predictXG(dataPath, fileName1, subsetSize = "10pc") # 
resultRF1 <- predictRF(dataPath, fileName1, subsetSize = "10pc") # 


# BOW 10pc # 15-07
fileName2 <- "BOW-10pc-FALSE.fst" 
indexName2 <- "BOW-Indexes-10pc-FALSE.fst"
resultXG2 <- predictXG(dataPath, fileName2,
                       indexName2, subsetSize = "10pc") # 0.399
resultRF2 <- predictRF(dataPath, fileName2,
                       indexName2, subsetSize = "10pc") # 0.443


# Sums of word2vec 10PC
fileName3 <- "W2V-10pc-50-FALSE.fst"
resultNN3 <- predictNN(dataPath, fileName3) # 
resultXG3 <- predictXG(dataPath, fileName3, subsetSize = "10pc") # 0.14
resultRF3 <- predictRF(dataPath, fileName3, subsetSize = "10pc") # 0.17

# Sums of word2vec 100PC
fileName4 <- "W2V-100pc-50-FALSE.fst"
resultXG4 <- predictXG(dataPath, fileName4, subsetSize = "100pc") # 18.6
resultRF4 <- predictRF(dataPath, fileName4, subsetSize = "100pc") # 

# Array word2vec 10PC
fileName5 <- "W2VArray-10pc-50-FALSE.rds"
resultCN <- predictCNN(dataPath, fileName5, subsetSize = "10pc") # 0.15

# Array word2vec 100PC
fileName6 <- "W2VArray-100pc-50-FALSE.rds"
resultCN <- predictCNN(dataPath, fileName6, subsetSize = "100pc") # 0.208




# Emb 100pc # 15-07
fileName7 <- "Emb-100pc-FALSE.fst "
indexName7 <- "Emb-Indexes-100pc-FALSE.fst"
resultEmb7 <- predictEmb(dataPath, fileName7,
                       indexName7, subsetSize = "10pc") # 0.551

# Emb 100pc # 15-07
fileName8 <- "BOW-100pc-FALSE.fst"
indexName8 <- "BOW-Indexes-100pc-FALSE.fst"
resultEmb8 <- predictXG(dataPath, fileName8,
                         indexName8, subsetSize = "100pc") # 

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

