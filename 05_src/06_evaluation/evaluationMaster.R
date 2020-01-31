# global parameters
fullWidth <- 15.49779
fullHeight <- fullWidth * 9/16
fullWidth2 <- 15.49779 * 3/4
fullHeight2 <- fullWidth * 9/29
outPath <- "03_computedData/06_evaluatedData/"

# preselection ####
evalPreselection <- evaluateData(inPath = "03_computedData/05_modelData/preselection/",
                                 outPath = outPath)
evalPreselection <- read.fst("03_computedData/06_evaluatedData/evaluationResult.fst",
                             as.data.table = TRUE)
evalReduced <- evalPreselection[, .(modelName, accuracy, f1_M, mlogloss)]
evalReduced[, LatexOutput := paste0("$", accuracy, "$ ewline $", 
                                    f1_M, "$ ewline $", mlogloss, "$ ")]

# evalFinalModels ####
evalFinalModels <- evaluateData(inPath = "03_computedData/05_modelData/finalModels/",
                                outPath = outPath)

print(xtable(evalFinalModels, label = "tab:finalEvaluation"), 
      include.rownames = FALSE)


# prob vs acc ####
ggObj <- compareProbVsAcc(inPath = "03_computedData/05_modelData/finalModels/")
ggObj

# accuracy by class ####
ggObj <- plotAccByClass(inPath = "03_computedData/05_modelData/finalModels/")
ggsave(filename = paste0(outPath, "accByClass.pdf"),
       plot = ggObj, width = fullWidth, height = fullHeight, 
       device = "pdf")

# neighborClasses ####
res <- identifyNeighborClasses("03_computedData/05_modelData/finalModels/")








# CNN Filters ####
res <- analyseCNNFilters(modelPath = "03_computedData/05_modelData/finalModels/mod_GloveArray300_CNNArray_10New_.RDS",
                         WEPath = "03_computedData/04_preparedData/GlovePure-10pc-300-FALSE.fst",
                         n_gram = 2)

saveRDS(res, file = paste0(outPath, "analyseCNNFilters.RDS"))
