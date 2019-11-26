indexes <- readRDS(paste0(inPath, "indexes",
                          subsetSize, ".rds"))


# HeadLShortD in if schleife rein


# IndexDT for knowing which observations belong in train and test set
IndexDT <- data.table(isFiltered = filter)[ , isTrainSet := .I %in% indexes]

# tokens <- tokens[filter] löschen

labelRed <- as.factor(labelRaw)


result <- data.table(labelRaw = as.factor(labelRed), tokens.dt)
rm(tokens.dt)

# todo hier ueberlegen ob das auch guenstiger geloest werden kann
resultTrain <- result[IndexDT$isFiltered & IndexDT$isTrainSet, ]
resultTest <- result[IndexDT$isFiltered & !IndexDT$isTrainSet, ]
assert(ncol(resultTrain) == ncol(resultTest))

labelTrain <- labelRed[IndexDT$isFiltered & IndexDT$isTrainSet]
labelTest <- labelRed[IndexDT$isFiltered & !IndexDT$isTrainSet]

labelTrain = labelTrain,
labelTest = labelTest,
#label in Train/test löschen

print(paste("check that train and test is rightly filtered: there are",
            - nrow(resultTrain) - nrow(resultTest) + nrow(subsetData),
            "deleted data points"))

saveRDS(list(resultTrain = resultTrain,
             resultTest = resultTest,
             IndexDT = IndexDT), 
        file = paste0(outPath, "BOW-", 
                      subsetSize, "-", saveSparse,
                      "-", mergeSD, ".rds"),
        compress = FALSE)