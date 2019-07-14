dataPath <- "03_computedData/04_preparedData/"
list.files(dataPath)
fileName8 <- "Emb-100pc-Binary.fst"
fileName <- fileName8
subsetSize <- "100pc"



# TFIDF 10pc # 13-07
fileName1 <- "TFIDF-10pc--FALSE.fst" 
resultXG1 <- predictXG(dataPath, fileName1, subsetSize = "10pc") # 0.400
resultRF1 <- predictRF(dataPath, fileName1, subsetSize = "10pc") # 0.437


# BOW 10pc # 12-07
fileName2 <- "BOW-10pc-FALSE.fst" 
resultNN2 <- predictNN(dataPath, fileName2) # 
resultXG2 <- predictXG(dataPath, fileName2, subsetSize = "10pc") # 0.400
resultRF2 <- predictRF(dataPath, fileName2, subsetSize = "10pc") # 0.433


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


for(file in grep(x = ls(), pattern = "result", value = TRUE)){
  saveRDS(get(file, inherits = FALSE),
          file = paste0("03_computedData/05_modelData/",
                                   file, ".RDS"))
}


# for(file in list.files("03_computedData/05_modelData/")){
#   assign(x = gsub(file (get(file, inherits = FALSE),
#           file = paste0("03_computedData/05_modelData/",
#                         file))
# }
