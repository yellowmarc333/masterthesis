#' @author Marc Schmieder
#' @description plots simple CeterisParibus Plot
#' @param model machine learning model
#' @param testMatrix matrix with test observations
#' @param idx number of row where the explaining observation is
#' @param column column name
#' @param method model method
#' @param rangeExtension default 1, adjust: see what model does when values are lower and higher 
#' @param length.out number of examples in range to compute
#' @return list of plot Data
singleCP = function(model, testMatrix, explainIDs, idx, column, method,
                      rangeExtension = 1, length.out = 100, multipleCT = F){
  setDT(testMatrix)

  # choose observation to explain
  observation = testMatrix[idx,]
  actualValue = observation[, get(column)]
  columnRange = base::range(testMatrix[, get(column)], na.rm = T)
  # for 0-1 variables where there are no 1 entries in test Data
  if(length(unique(testMatrix[, get(column)])) <= 2) columnRange = c(0,1)

  # make a sequence vector with entries in the range of the variable
  valuesToRange = seq(from = columnRange[1] * rangeExtension, 
                      to = columnRange[2]* rangeExtension,
                      length.out = length.out)
  
  # modify this observation with the sequence vector
  newDataModif = observation[rep(1, length(valuesToRange)),]
  newDataModif[, c(column) := valuesToRange]
  
  # setup predictions with dependent on the model.
  if(method == "logreg") {
    predictions = predict(model , as.matrix(newDataModif), proba = T)$probabilities[,"TRUE"]
    actualPred = predict(model , as.matrix(observation), proba = T)$probabilities[,"TRUE"]
  }
  if(method == "decisionTree"){
    predictions = predict(model , as.matrix(newDataModif))[, 2]
    actualPred = predict(model , as.matrix(observation))[, 2]
  } 
  if(method == "xgBoost") {
    newDataModif = newDataModif[, .SD, .SDcols = colnames(newDataModif)[colnames(newDataModif) %in% model$feature_names]]
    predictions = predict(object  = model, newdata = as.matrix(newDataModif[, .SD, .SDcols = model$feature_names]))
    actualPred = predict(object  = model, newdata = as.matrix(observation[, .SD, .SDcols = model$feature_names]))
  }
  if(method == "randomForest"){
     predictions = predict(model, newDataModif)$predictions
     actualPred = predict(model, observation)$predictions
  }
  
  plotData = data.table(valuesToRange = valuesToRange, predictions = predictions)
  plotData = plotData[complete.cases(plotData)]
  
  # here is an alternative return option (used in other function multipleCP)
  if(multipleCT){
    plotData[, valueDiff := (valuesToRange - actualValue)/diff(columnRange)]
    plotData[, predDiff := (predictions - actualPred)]
    plotData[, changeInfluence := predDiff/valueDiff]
    return(list(predDiff = plotData$predDiff,
                valuesToRange = valuesToRange,
                actualValue = actualValue,
                changeInfluence = plotData$changeInfluence,
                predictions = predictions))
  }
  
  # data.table for coordinates for geom_label
  annotateData3 = data.table(actualValue = as.numeric(actualValue),
                            yValue = stats::median(plotData$predictions),
                            label = "tatsächlicher Wert")
  
  ggObj = ggplot(plotData, aes(x = valuesToRange, y = predictions)) +
    geom_line() +
    geom_vline(xintercept = as.numeric(actualValue), color = "lightgreen") +
    geom_label(data = annotateData3, aes(x = actualValue, y = yValue, label = label)) +
    labs(title = paste0("Änderung der Ausschusswahrscheinlichkeit \n mit der Variablen ",
                       column, "\n für eine Beobachtung"),
         x = paste0("Variable ", column),
         y = paste0("Ausschusswahrscheinlichkeit"),
         color = paste0("tatsächlicher Wert")) +
    wdl_theme
  
  return(list(ggObj = ggObj))
}


#' @author Marc Schmieder
#' @description plots multiple CeterisParibus Plot
#' @param model machine learning model
#' @param testMatrix matrix with test observations
#' @param idx number of row where the explaining observation is
#' @param column column name
#' @param method model method
#' @param length.out number of examples in range to compute

#' @return list of plot Data
multipleCP = function(model, testMatrix, explainIDs, idx, column, method,
                               length.out = 100){
  
  # compute data from singleCP function for sampled observations
  multipleCTData = sapply(as.list(explainIDs),  function(x){
    singleCP(model = model, testMatrix = testMatrix, idx = x, column = column, length.out = length.out,
               multipleCT = T, method = method)$predictions
  })
  
  # compute standard deviations and means
  rSDs = apply(multipleCTData, 1, sd)
  rMeans = rowMeans(multipleCTData)
  # get x-axis values from singleCP
  valuesToRange = singleCP(model = model, testMatrix = testMatrix, idx = explainIDs[1],
                             column = column, length.out = length.out, method = method,
                             multipleCT = T)$valuesToRange
  
  plotData = as.data.table(multipleCTData)
  setnames(plotData, names(plotData), paste0(1:length(explainIDs)))
  plotData[, valuesToRange := valuesToRange]
  plotData[, rMeans := rMeans]
  plotData[, rSDs := rSDs]
  ## adjust: remove plotting of means and SDs, undo later and add legends
  plotData[, rMeans := NULL]
  plotData[, rSDs := NULL]
 
  plotData = plotData[complete.cases(plotData)]
  # melted Data for ggplot
  plotDataLong <- melt(plotData, id= "valuesToRange", variable.name = "Datenpunkt")  # convert to long format
  
  ggObj = ggplot(data = plotDataLong, aes(x= valuesToRange, y=value, colour = Datenpunkt)) +
    geom_line() +
    labs(title = paste("Änderung der Ausschusswahrscheinlichkeit \n mit der Variablen" , column), 
         subtitle = paste("Modell:", method),
         x = column, 
         colour = "zufällig ausgewählte \n Beobachtungen",
         y = "Ausschusswahrscheinlichkeit") 

  return(list(ggObj = ggObj,
              plotData = plotData))
}

#' @author Marc Schmieder
#' @description computes data for 3 plot objects
#' @param model machine learning model
#' @param testMatrix matrix with test observations
#' @param testLabel vector with label
#' @param column column name
#' @param method model method
#' @param length.out number of examples in range to compute
#' @return list of plot Data
plotVariableVsPredAndClass = function(model, testData, testLabel , column, 
                                     method = "logisticRegression"){
  
  setDT(testData)
  # get entries of column, if there are more than 40 distinct uniques, then set the class as numeric(for later plots)
  values = testData[, get(column)]
  if(length(unique(values)) >= 40) values =  as.numeric(values)
  
  # get predictions dependend of method
  if(method == "logisticRegression") predictions = predict(model , testData, proba = T)$probabilities[,"TRUE"]
  if(method == "decisionTree"){
    predictions = predict(model , testData)[, 2]
  } 
  if(method == "xgBoost") {
    testData = testData[, .SD, .SDcols = colnames(testData)[colnames(testData) %in% model$feature_names]]
    predictions = predict(object  = model, newdata = as.matrix(testData[, .SD, .SDcols = model$feature_names]))
  }
  if(method == "randomForest"){
    predictions = predict(model, testData)$predictions
  }
  
  #------------------------------------ first  plot ---------------------------------#
  
  plotDT = data.table(values = values, predictions = predictions, labels = testLabel)
  plotDT[, classCount := .N, by = values]
  plotDT = plotDT[complete.cases(plotDT)]
  # data.table for annotation
  annotateData = plotDT[, .(classCount = unique(classCount),
                            quantile = stats::quantile(predictions, probs = 0.75, na.rm=T) + 0.05), by = values]
  ggObj = ggplot(plotDT, aes(x = values, y = predictions)) 
  
  # initialise list for json
  frontendlist = list()
  
  # from here the ggObj is dependend of the class of the column
  # case 1
  NValues <- length(unique(values))
  if(is.logical(values) | (is.integer(values) & NValues < 5) |
     is.character(values) | is.factor(values)){
    # get boxplot values
    print(paste0("Box:", column))
    frontendlist=buildBoxplotStatsValues(plotDT, column)
    #create a boxplot
    ggObj = ggObj + geom_boxplot(mapping = aes(x = as.factor(values), y = predictions, 
                                               fill = "#ED6C42")) +
      geom_label(data = annotateData , aes(x = as.factor(values), y = quantile, label = classCount))
  } 
  # case 2
  else if(is.numeric(values) & 
          !(is.integer(values) & NValues < 5)) {
    # get scatter values
    print(paste0("Scatter:", column))
    frontendlist=buildScatterplotList(plotDT, column)
    lmModel=lm(predictions ~ values,plotDT)
    valueMin=min(plotDT$values,na.rm=T)
    valueMax=max(plotDT$values,na.rm=T)
    line=data.table(x=c(valueMin,valueMax))
    line[, y:=lmModel$coefficients[2]*x+lmModel$coefficients[1]]
    line=lapply(1:2,function(x){return(unlist(unname(line[x])))})
    frontendlist=c(frontendlist,line = list(line))
    # create scatter plot
    ggObj = ggObj + geom_smooth(method = "lm", formula = y ~ x) + geom_point()
  }
  # in any case add labs and themes
  ggObj = ggObj +
    labs(title = paste("Variable", column, "und die dazugehörige Modellprognose"), 
         subtitle = paste("Modell:", method),
         x = column, 
         y = "Modellprognose") +
    wdl_theme +
    theme(legend.position = "none")

  #------------------------------------ second plot ---------------------------------#
  annotateData2 = plotDT[, .(classCount = unique(.N),
                             quantile = stats::quantile(values, probs = 0.75, na.rm=T) + 0.05), by = labels]

  # get boxplot values for plot class vs variable
  frontendlist2 = buildBoxplotStatsLabel(plotDT, column)

  ggObj2 = ggplot(plotDT) +
    # geom_boxplot(mapping = aes(x = as.factor(as.integer(labels)), y = values, 
    #                           fill = "#ED6C42")) +
    geom_point(mapping = aes(x = labels, y = values))  +
    # geom_label(data = annotateData2 , aes(x = as.factor(as.integer(labels)), 
    #                                       y = quantile, label = classCount)) 
    labs(title = paste0("Variable ", column, ": Ausprägungen je Klasse der Zielvariable"), 
         subtitle = paste("Modell:", method),
         x = "Zielvariable", 
         y = paste(column)) +
    coord_flip() +
    wdl_theme +
    theme(legend.position = "none")
  
  #--------------------- third  plot (combination from first 2) ---------------#
  ggObj3 = ggarrange(plotlist = list(ggObj, ggObj2), ncol = 2, nrow = 1)
  
  return(list(ggObj = ggObj,
              ggObj2 = ggObj2,
              ggObj3 = ggObj3,
              frontendlist = frontendlist,
              frontendlist2 = frontendlist2))
  
}



#' @author Marc Schmieder
#' @description computes betaCoeficcients of Liblinear Models with Type = 6
#' @param model model list
#' @param n_feat  number of features for which the plot should be computed
#' @param trainLabel only binary Label
#' @return data.table with columns coefficients, variable name and beta coefficients
getTopImportantFeatures = function(model, n_feat = 10, method = "logisticRegression", explainTrainData,
                                   explainTrainLabel, onlyFeatImp = F){
  
  assertChoice(method, choices = c("logisticRegression", 
                                   "decisionTree", "xgBoost", "randomForest"))
  
  if(method == "logisticRegression"){
    coefficientsDT = betaCoefLiblineaR(model = model, trainData = explainTrainData, trainLabel = explainTrainLabel)
    TopImportantFeatures = coefficientsDT$Variable[1: n_feat]
    TopImportantValues = scaleInRange(abs(coefficientsDT$beta[1: n_feat]), c(0,1))
    TopImportantValuesNotStd = abs(coefficientsDT$beta[1: n_feat])
    yTitle = "absolute beta Koeffizienten"
  }
  if(method == "decisionTree"){
    table = model$frame[, c("var", "dev")]
    setDT(table)
    setorderv(table, "dev", "-1")
    TopImportantFeatures = unique(table$var)[1:min(n_feat, length(unique(table$var)))]
    TopImportantFeatures = as.character(TopImportantFeatures[TopImportantFeatures != "<leaf>"])
    indizes = apply(as.data.frame(TopImportantFeatures), 1, function(x) which(table$var == x)[1])
    TopImportantValues = scaleInRange(table$dev[indizes], c(0,1))
    TopImportantValuesNotStd = table$dev[indizes]
    yTitle = "dev in split"
  }
  
  if(method == "xgBoost"){
    
    VarImpXgb = xgb.importance(model = model)
    #plotObj = xgb.plot.importance(VarImpXgb)
    TopImportantFeatures = VarImpXgb$Feature[1:min(c(n_feat, nrow(VarImpXgb)))]
    TopImportantValues = scaleInRange(VarImpXgb$Gain[1:min(c(n_feat, nrow(VarImpXgb)))], c(0,1))
    TopImportantValuesNotStd = VarImpXgb$Gain[1:min(c(n_feat, nrow(VarImpXgb)))]
    yTitle = "gain"
  }
  
  if(method == "randomForest"){
    VarImpRF = importance(x = model)[order(importance(x = model), decreasing = T)]
    TopImportantFeatures = names(VarImpRF)[1:min(c(n_feat, length(VarImpRF)))]
    TopImportantValues = scaleInRange(VarImpRF[1:min(c(n_feat, length(VarImpRF)))], c(0,1))
    TopImportantValuesNotStd = VarImpRF[1:min(c(n_feat, length(VarImpRF)))]
    yTitle = "impurity"
  }
  
  plotData = data.table(Variablen = TopImportantFeatures, Value = TopImportantValues)
  
  ggObj = ggplot(plotData, aes(x = reorder(Variablen, Value), y = Value)) +  
    geom_col(fill = wdlBlue[4]) +
    labs(title = paste("Variablenwichtigkeit"), 
         subtitle = paste("Modell:", method),
         y = "Variablenwichtigkeit",
         x = "Variablen") +
    geom_text(aes(label = round(Value, 3))) +
    coord_flip() +
    theme(legend.position = "none")
  
  # compute the data to a json file
  plotDataJson = jsonlite::toJSON(head(plotData, n=10L))
 
  # second plot starting here
  plotData2 = data.table(Variablen = TopImportantFeatures, Value = TopImportantValuesNotStd)
  
  if(onlyFeatImp){
    fwrite(plotData, file = paste0(pathToSave ,"/variable_table_", method, ".csv"))
    return(TopImportantFeatures)
  }

  ggObj2 = ggplot(plotData2, aes(x = reorder(Variablen, Value), y = Value)) +  
    geom_col(fill = wdlBlue[4]) +
    labs(title = paste("Variablenwichtigkeit"), 
         subtitle = paste("Modell:", method),
         y = yTitle,
         x = "Variablen") +
    geom_text(aes(label = round(Value, 3))) +
    coord_flip() +
    wdl_theme +
    theme(legend.position = "none")
  
  
  
  # compute the data to a json file
  plotDataJson2 = jsonlite::toJSON(head(plotData2,n=10L))
  
  return(list(TopImportantFeatures = TopImportantFeatures,
              ggObj = ggObj,
              ggObj2 = ggObj2,
              plotDataJson = plotDataJson,
              plotDataJson2 = plotDataJson2))
}

buildBoxplotStatsValues <- function(DT, column){
  values=unique(DT[!is.na(values)]$values)
  h=lapply(values,function(x){
    bpStats=boxplot.stats(DT[values==x]$predictions)
    hx=c(name=as.character(as.integer(x)),
        n=bpStats$n,
        max=bpStats$stats[[5]],
        min=bpStats$stats[[1]],
        median=bpStats$stats[[3]],
        lower=bpStats$stats[[2]],
        upper=bpStats$stats[[4]],
        outlier=list(bpStats$out))
    return(hx)})
  res=c(feature=column,type="boxplot", data=list(h))
  return(res)
}

buildBoxplotStatsLabel <- function(DT, column){
  labels=unique(DT[!is.na(labels)]$labels)
  h=lapply(labels,function(x){
    bpStats=boxplot.stats(as.integer(DT[labels==x]$values))
    hx=c(name=as.character(as.integer(x)),
         n=bpStats$n,
         max=bpStats$stats[[5]],
         min=bpStats$stats[[1]],
         median=bpStats$stats[[3]],
         lower=bpStats$stats[[2]],
         upper=bpStats$stats[[4]],
         outlier=list(bpStats$out))
    return(hx)})
  res=c(feature=column,type="boxplot", data=list(h))
  return(res)
}

buildScatterplotList <- function(DT,column) {
  data=DT[,.(values,predictions)]
  hx = list()
  for (i in 1:nrow(data)) {
    hx = c(hx, list(unlist(unname(as.list(data[i,])))))
  }
  res=c(feature=column,type="scatterplot", data=list(hx))
  return(res)
}


#' @author Marc Schmieder
#' @description plots simple pdp plot
#' @param model machine learning model
#' @param trainData trainData
#' @param column column name
#' @param method model method
#' @return list of plot Data
pdpSingle = function(model, trainData, column, method){
  assertDataTable(trainData)
  assertString(column)
  assertString(method)
  
  if (method == "xgBoost") {
    res <- pdp::partial(model, pred.var = column, 
                        train = trainData, type = "regression",
                        plot = TRUE, rug = TRUE, plot.engine = "ggplot2",
                        ice = FALSE)

  }
  return(res)
}


#' @author Marc Schmieder
#' @description plots simple pdp plot
#' @param model machine learning model
#' @param trainData trainData
#' @param column1 feature name 1
#' @param column2 feature name 2
#' @param method model method
#' @return list of plot Data
pdpTupel <- function(model, trainData, column1, column2, method,
                     labelName = "yhat"){
  assertDataTable(trainData)
  assertString(column1)
  assertString(column2)
  assertString(method)
  
  if (method == "xgBoost") {
    res <- pdp::partial(model, pred.var = c(column1, column2),
                   plot = TRUE, chull = TRUE, plot.engine = "ggplot2",
                   train = trainData, type = "regression")
    # renaming the yhat variable (complicated, could be done easier)
    res[["data"]][[labelName]] <- res[["data"]][["yhat"]]
    res[["data"]][["yhat"]] <- NULL
    # res <- pdp::partial(model, pred.var = c(column1, column2), chull = TRUE,
    #                     train = trainData, type = "regression")
    # res2 <- plotPartial(res, contour = TRUE,
    #                    plot.engine = "ggplot2")

    ggObj <-suppressMessages(plot(res, contour = TRUE) + 
      scale_fill_continuous(name = labelName,  type = "viridis") +
      labs(title = paste("Einfluss von Variable", column1, "und Variable",
                         column2, "auf die Modellprognose"),
           subtitle = paste("Modell:", method),
           fill = "test"))

    # res3 <- plotPartial(res, levelplot = FALSE, zlab = "cmedv", colorkey = TRUE, 
    #                     screen = list(z = -20, x = -60))
  }
  return(ggObj)
}


limeObsFeat <- function(model, trainData, explainData,
                        n_labels = 1, n_features = 2, method) {
  assertDataTable(explainData)
  assertNumber(n_labels)
  assertNumber(n_features)
  assertString(method)
  
  if (method == "xgBoost") {
    explanation <- lime(trainData, model)
    explanations <- explain(explainData, explanation,
                            n_labels = n_labels, n_features = n_features)
    res <- plot_explanations(explanations)
  }
  
  return(res)
}



