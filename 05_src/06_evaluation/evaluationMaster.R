# evaluation/explaining

# exchange words and see what happens, if the prediction gets into nearest
# input sequence of text (first first word, then 2nd word etc)
# schaue benachbarte klassen an, in die am meisten falsch klassifiziert wird.

# global parameters
fullWidth <- 15.49779
fullHeight <- fullWidth * 9/16
fullWidth2 <- 15.49779 * 3/4
fullHeight2 <- fullWidth * 9/29
outPath <- "03_computedData/06_evaluatedData/"

compareProbVsAcc(inPath = "03_computedData/05_modelData/finalselection/")

ggObj <- plotAccByClass(inPath = "03_computedData/05_modelData/finalselection/")
ggsave(filename = paste0(outPath, "accByClass.pdf"),
       plot = ggObj, width = fullWidth, height = fullHeight, 
       device = "pdf")
