dataPath <- "03_computedData/04_preparedData/"
fileName1 <- "News0.01Subset.fst"
fileName2 <- "News0.1Subset.fst"
fileName3 <- "W2V0.01Subset.fst"

resultNN <- predictNN(dataPath, fileName1, trainRatio = 0.75)
resultXG <- predictXG(dataPath, fileName1, trainRatio = 0.75)
resultRF <- predictRF(dataPath, fileName1, trainRatio = 0.75)

resultNN2 <- predictNN(dataPath, fileName2, trainRatio = 0.75)
resultXG2 <- predictXG(dataPath, fileName2, trainRatio = 0.75)
resultRF2 <- predictRF(dataPath, fileName2, trainRatio = 0.75)
# 2 stundne laufzeit auf 10% der daten.

resultNN3 <- predictNN(dataPath, fileName3, trainRatio = 0.75)
resultXG3 <- predictXG(dataPath, fileName3, trainRatio = 0.75)
resultRF3 <- predictRF(dataPath, fileName3, trainRatio = 0.75)