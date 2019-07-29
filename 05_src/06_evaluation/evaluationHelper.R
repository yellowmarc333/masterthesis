compareProbVsAcc <- function(data, cutN = 100) {

  ProbAccDT <- data[["ProbAccDT"]]
  ProbAccDT[, ProbCut := cut(x = Prob, breaks = cutN)]

  plotDT <- ProbAccDT[, .(ProbMean = mean(Prob),
                CorrectMean = mean(Correct)), by = ProbCut]
  setorderv(plotDT, "ProbCut", "1")
  plotDT[, CutLvl := 1: .N]
  
  ggObj <- ggplot(plotDT, aes(x = CutLvl)) + 
    geom_line(aes(y = CorrectMean), color = "green") +
    geom_line(aes(y = ProbMean), color = "blue")
  
  return(ggObj)
}

compareProbVsAcc(data = resultEmb, cutN = 50)
