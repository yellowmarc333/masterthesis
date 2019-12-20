#' @author Marc Schmieder
#' @description computes betaCoeficcients of Liblinear Models with Type = 6
#' @param model
#' @param trainData
#' @param trainLabel only binary Label
#' @return data.table with columns coefficients, variable name and beta coefficients

betaCoefLiblineaR <- function(model, trainData, trainLabel) {
  
  coefficientsDT = data.table(Coefficients = model$W[1, ], Variable = colnames(model$W))
  SxDivSy = sapply(trainData, function(x) {sd(x, na.rm = T)})/sd(trainLabel)
  rightSideDT = data.table(SxDivSy = SxDivSy, Variable = names(SxDivSy))
  
  joinDT = merge(coefficientsDT, rightSideDT, by = "Variable", all.x = T)
  joinDT[, beta := Coefficients*SxDivSy]
  joinDT = joinDT[order(abs(beta), decreasing = T)]
  
  return(joinDT)
  
}

