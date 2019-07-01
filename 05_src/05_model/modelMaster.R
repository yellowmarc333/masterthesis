dataPath <- "03_computedData/04_preparedData/"
list.files(dataPath)
fileName1 <- "News0.01Subset.fst"
fileName2 <- "News0.1Subset.fst"

fileName3 <- "W2V0.01Subset.fst"
fileName4 <- "W2V-0.1-100.fst"
fileName5 <- "W2V-0.1-500.fst"
fileName6 <- "W2V-0.1-50.fst"
fileName7 <- "BOW-0.01-TRUE.fst"

resultNN <- predictNN(dataPath, fileName1, trainRatio = 0.75) #0.29
resultXG <- predictXG(dataPath, fileName1, trainRatio = 0.75) #0.269
resultRF <- predictRF(dataPath, fileName1, trainRatio = 0.75) #0.33

# with shortdescription
resultNN7 <- predictNN(dataPath, fileName7, trainRatio = 0.75) # 0.274
resultXG7 <- predictXG(dataPath, fileName7, trainRatio = 0.75) # 0.356
resultRF7 <- predictRF(dataPath, fileName7, trainRatio = 0.75) # 0.386

resultNN2 <- predictNN(dataPath, fileName2, trainRatio = 0.75) #0.389
resultXG2 <- predictXG(dataPath, fileName2, trainRatio = 0.75) #0.413
resultRF2 <- predictRF(dataPath, fileName2, trainRatio = 0.75) # 0.456
# 2 stundne laufzeit auf 10% der daten.

# from here on word2vec
resultNN3 <- predictNN(dataPath, fileName3, trainRatio = 0.75) # 0.15
resultXG3 <- predictXG(dataPath, fileName3, trainRatio = 0.75) # 0.19
resultRF3 <- predictRF(dataPath, fileName3, trainRatio = 0.75) # 0.21

resultNN4 <- predictNN(dataPath, fileName4, trainRatio = 0.75) # 0.244
resultXG4 <- predictXG(dataPath, fileName4, trainRatio = 0.75) # 0.231
resultRF4 <- predictRF(dataPath, fileName4, trainRatio = 0.75) # 0.243

resultNN5 <- predictNN(dataPath, fileName5, trainRatio = 0.75) # 0.243
resultXG5 <- predictXG(dataPath, fileName5, trainRatio = 0.75) # 0.21
resultRF5 <- predictRF(dataPath, fileName5, trainRatio = 0.75) # 0.21
# word vector mit zu vielen dimensionen bringt nichts

resultNN6 <- predictNN(dataPath, fileName6, trainRatio = 0.75) # 0.25
resultXG6 <- predictXG(dataPath, fileName6, trainRatio = 0.75) # 0.238
resultRF6 <- predictRF(dataPath, fileName6, trainRatio = 0.75) # 0.253

for(file in grep(x = ls(), pattern = "result", value = TRUE)){
  saveRDS(get(file, inherits = FALSE),
          file = paste0("03_computedData/05_modelData/",
                                   file, ".RDS"))
}

einlesen
# for(file in list.files("03_computedData/05_modelData/")){
#   assign(x = gsub(file (get(file, inherits = FALSE),
#           file = paste0("03_computedData/05_modelData/",
#                         file))
# }
