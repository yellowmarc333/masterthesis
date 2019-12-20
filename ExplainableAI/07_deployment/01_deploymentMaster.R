#' @author Marc Schmieder
#' @description/Disclaimer
#' Please provide only path with rds files of one type ("regression" or "timeseries")
#' This function expects rds input files with the following structure:
#' - 1. level is the datasets (different filters, in this case only 1)
#' - 2. level are the predictor models
#' predictor model has the structure provided by modelData() containing also
#' type and labelName
#' 
#'         generates plots and json Data in deployment Data: feature importance, scatter/boxplots, 
#'         and explaining for selected Models: (at the moment xgboost)
#'         This functions works for methods "xgboost" "randomforest", "logisticRegression", 
#'         decisionTree might throw some errors.
#'         This function can be recycled to other projects. Search for 'adjust' and see where you have
#'         to make changes.
#'         All the functions used in explainData are located in 07_plottingFunctions.R, for adding plots write a function in this skript
#' @param dataPath directory in which the data is located
#' @param targetPath directory for saving modelled data#
#' @param nfeat number of top features the plots and variable importance should be generated
#' @param n_explain for multiple ceteris paribus plots: how many observations should be plotted
#' @param explainIndex for single ceteris paribus: which observation to explain. Default always the
#'        first observation of testData. This feature can and should be extended in die future
#' @param mode ignore
#' @param onlyFeatImp ignore
#' @param feature_importance should plots be generated?
#' @param simple_model should simple model be calculated?
#' @param single_plots should plots be generated?
#' @param pdp_tupel_plots should plots be generated?
#' @param single_ceterisParibus should plots be generated?
#' @param multiple_ceterisParibus should plots be generated?
#' @param waterfall should plots be generated?
#' @return Nothing. writes files as pngs 
explainModel = function(dataPath = "03_computedData/05_modelData/",
                       targetPath = "03_computedData/07_deploymentData/",
                       n_feat = 10, n_explain = 50, explainIndex = 1,
                       mode = "fold", onlyFeatImp = FALSE, 
                       feature_importance = FALSE, simple_model = FALSE,
                       single_plots = FALSE, pdp_tupel_plots = FALSE,
                       single_ceterisParibus = FALSE, 
                       multiple_ceterisParibus = FALSE, waterfall = FALSE) {
  assertString(dataPath)
  assertString(targetPath)
  assertNumber(n_feat)
  assertNumber(n_explain)
  assertNumber(explainIndex)
  assertString(mode)
  assertFlag(feature_importance)
  assertFlag(simple_model)
  assertFlag(single_plots)
  assertFlag(pdp_tupel_plots)
  assertFlag(single_ceterisParibus)
  assertFlag(multiple_ceterisParibus)
  
  # list allFiles
  allFiles = list.files(dataPath)
  
  # select subset of allFiles, adjust for different pattern
  files = allFiles
  
  # 1. for loop: for every dataset in modeldata, create deployment
  for(file in files) {
    # naming the current dataSet
    dataSet = gsub(strsplit(file, split = "_")[[1]][2], pattern = ".rds", 
                   replacement = "")
    print(paste("======== deploying dataSet", dataSet, "========"))
    # read in the 
    modelResults = readRDS(file = paste0(dataPath, file))

    
    # 2. for loop: for every method (e.g. xgboost or logisticRegression) create deployment
    for(method in names(modelResults)){
      print(paste("======== deploying method" ,method, "========"))

      type <- modelResults[[method]][["type"]]
      labelName <- modelResults[[method]][["labelName"]]
  
      # read in from modelResults list
      wholeData = modelResults[[method]]$predictions$data
      actuals = modelResults[[method]]$predictions$actuals
      model1 = modelResults[[method]]$models[[1]]
      
      explainTrainData = model1$trainData
      explainTrainLabel = model1$trainLabel
      explainTestData = model1$testData
      explainTestLabel = model1$testLabel
      explainModel = model1$model
      
      # set as all feature if TopImportantFeatures shouldnt be calculated
      TopImportantFeatures <- colnames(explainTrainData)
 
      # # --------------------- feature importance -------------------------------------#
      # compute plot and json data
      if(feature_importance & type != "timeseries") {
        functionOutput = getTopImportantFeatures(model = explainModel, 
                                                 n_feat = n_feat,
                                                 method = method,
                                                 explainTrainData = explainTrainData, 
                                                 explainTrainLabel = explainTrainLabel,
                                                 onlyFeatImp = onlyFeatImp)
        # character vector of feature names
        TopImportantFeatures = functionOutput$TopImportantFeatures
        
        ## saving output,  adjust foldername 
        pathToSave = paste0(targetPath, dataSet,"/", method, "/variable_importance")
        dir.create(pathToSave, showWarnings = FALSE, recursive = TRUE)
        
        # pngs
        ggsave(functionOutput$ggObj, filename = paste0("variable_importance_", method, ".png"), device = "png",
               path = pathToSave, width = 10, height = 7.5)
        ggsave(functionOutput$ggObj2, filename = paste0("variable_importanceNotStd", method, ".png"), device = "png",
               path = pathToSave, width = 10, height = 7.5)
        # # json
        # readr::write_lines(functionOutput$plotDataJson, paste0(pathToSave,"/","variable_importance_", method, ".json"))
        # readr::write_lines(functionOutput$plotDataJson2, paste0(pathToSave,"/","variable_importanceNotStd_", method, ".json"))
        
      }
     
      if (simple_model & type != "timeseries") {
        #---------- compute decision tree on top important features----------####
        
        topFeatureTrainData <- explainTrainData[ , .SD, 
                                                 .SDcols = TopImportantFeatures]
        topFeatureTestData <- explainTestData[, .SD, 
                                              .SDcols = TopImportantFeatures]
        simpleModelRes <- simpleModelTree(trainData = topFeatureTrainData,
                                          trainLabel = explainTrainLabel,
                                          testData = topFeatureTestData,
                                          testLabel = explainTestLabel)
        predict(model1$model, newdata = as.matrix(explainTestData))
        rmse_method <- sqrt(mean((predict(model1$model, 
                                          newdata = as.matrix(explainTestData))
                                  - explainTestLabel)^2))
        print(paste("model rmse is", rmse_method))
        print(paste("simple model rmse is", simpleModelRes$rmse))
        
        # saving plot
        pathToSave = paste0(targetPath, dataSet,"/", method, "/SimpleModels/")
        dir.create(pathToSave, showWarnings = F, recursive = T)
        pdf(paste0(pathToSave, "Tree_", method, ".pdf"),
            width = 10, height = 10) 
        # 2. Create a plot
        plot(x = simpleModelRes$model, type = "proportional")
        text(simpleModelRes$model, digits = 2)
        # Close the pdf file
        dev.off() 
      }
     
      #--------------- lime feature plot for some obversations -------------####
      # res <- limeObsFeat(model1$model, trainData = explainTrainData,
      #             explainData = explainTestData,
      #             n_labels = 1, n_features = n_feat, method) 
      
      ##------------- from here on different plots are computed for top important features----------------------------##
      if (single_plots & type != "timeseries") {
        # scatterplots: --------#
        # 1: variables against model prediction 
        # 2: variables against label 
        
        for(column in TopImportantFeatures) {
          FeatImpRank = which(TopImportantFeatures == column)
          FeatImpRank = ifelse(FeatImpRank/10 < 1, paste0(0, FeatImpRank), 
                               paste0(FeatImpRank))
          
          # compute plot and json data
          functionOutput = plotVariableVsPredAndClass(model = explainModel, 
                                                      testData = wholeData,
                                                      testLabel = actuals , 
                                                      column = column, method = method)
          ### saving output,  adjust foldername
          pathToSave = paste0(targetPath, dataSet,"/", method, "/prediction_plots")
          dir.create(pathToSave, showWarnings = F, recursive = T)
          pathToSave2 = paste0(targetPath, dataSet,"/", method, "/classDistribution")
          dir.create(pathToSave2, showWarnings = F, recursive = T)
          pathToSave3 = paste0(targetPath, dataSet,"/", method, "/combined")
          dir.create(pathToSave3, showWarnings = F, recursive = T)
          # pngs
          ggsave(functionOutput$ggObj, filename = paste0(FeatImpRank, "_", column, "_prediction_plots", ".png"), device = "png",
                 path = pathToSave, width = 10, height = 7.5)
          ggsave(functionOutput$ggObj2, filename = paste0(FeatImpRank, "_", column, "_classDistribution", ".png"), device = "png",
                 path = pathToSave2, width = 10, height = 7.5)
          ggsave(functionOutput$ggObj3, filename = paste0(FeatImpRank, "_", column, "_combined", ".png"), device = "png",
                 path = pathToSave3, width = 20, height = 7.5)
          
          # # json
          # readr::write_lines(jsonlite::toJSON(functionOutput$frontendlist, auto_unbox = T),paste0(pathToSave, "/", FeatImpRank, "_", column, "_prediction_plots", ".json"))
          # readr::write_lines(jsonlite::toJSON(functionOutput$frontendlist2, auto_unbox = T),paste0(pathToSave2, "/", FeatImpRank, "_", column, "_classDistribution", ".json"))
          
          # partial dependence plots ---------------------------------------####
          functionOutput <- pdpSingle(model1$model, explainTrainData,
                                      column, method)
          pathToSave = paste0(targetPath, dataSet,"/", method, "/pdpSingle")
          
          dir.create(pathToSave, showWarnings = F, recursive = T)
          ggsave(functionOutput, filename = paste0(FeatImpRank, "_", column, 
                                                   "_pdpSingle", ".png"), 
                 device = "png",
                 path = pathToSave, width = 10, height = 7.5)
          
        }
      }
     
      
      if (pdp_tupel_plots & type != "timeseries") {
        # tupels of pdp plots
        tupelCombn <- data.table(t(combn(TopImportantFeatures, 2)))
        setnames(tupelCombn, c("col1", "col2"))
        for (i in seq_len(nrow(tupelCombn))) {
          column1 <- tupelCombn[i, col1]
          column2 <- tupelCombn[i, col2]
          
          FeatImpRank1 = which(TopImportantFeatures == column1)
          FeatImpRank2 = which(TopImportantFeatures == column2)
          # for better display in finder/explorer
          FeatImpRank1 = ifelse(FeatImpRank1 < 10, 
                                paste0(0, FeatImpRank1),
                                paste0(FeatImpRank1))
          FeatImpRank2 = ifelse(FeatImpRank2 < 10,
                                paste0(0, FeatImpRank2), 
                                paste0(FeatImpRank2))
          

          functionOutput <- pdpTupel(model1$model,
                                     trainData = explainTrainData,
                                     column1 = column1,
                                     column2 = column2, method = method,
                                     labelName = labelName)
          
          pathToSave = paste0(targetPath, dataSet,"/", method, "/pdpTupel")
          
          dir.create(pathToSave, showWarnings = F, recursive = T)
          print(paste("saving pdp tupel:", column1, ",", column2))

          ggsave(functionOutput, 
                 filename = paste0(FeatImpRank1, "_", column1, "_",
                                   FeatImpRank2, "_", column2, 
                                   "_pdpTupel", ".png"), 
                 device = "png",
                 path = pathToSave, width = plotWidthNarrow, height = plotHeight)
        }
      }
     
      if (type == "timeseries" & method == "prophet") {
        future <- prophet::make_future_dataframe(explainModel, periods = 365)
        forecast <- predict(explainModel, future)
        ggObj <- plot(explainModel, forecast)
        
        ggObj2 <- prophet_plot_components(explainModel, forecast)
       
        pathToSave = paste0(targetPath, dataSet,"/", method, "/prophetPlots/")
        dir.create(pathToSave, showWarnings = F, recursive = T)
        print(paste("saving prophet plots:"))
        
        ggsave(ggObj, 
               filename = paste0("prophetForecast.png"), 
               device = "png",
               path = pathToSave, width = plotWidthNarrow, height = plotHeight)
        for(i in seq_len(length(ggObj2))) {
          ggsave(ggObj2[[i]], 
                 filename = paste0("prophetComponents", i ,".png"), 
                 device = "png",
                 path = pathToSave, width = plotWidthNarrow, height = plotHeight)
        }
      }
      
      ####------ Explaining for selected Methods (at the moment only xgBoost) -#
      if(method %in% c("xgBoost")){
        
        # sample from test data n_explain observations to draw in 
        # multiple Ceteris Paribus plot
        explainIDs = sample.int(nrow(explainTestData), n_explain)
        
        
        #-------------Single Ceterus Paribus plots ----------------------------#
        if (single_ceterisParibus & type != "timeseries") {
          for (column in TopImportantFeatures) {
            print(paste0("single CP for " ,column))
            FeatImpRank = which(TopImportantFeatures == column)
            FeatImpRank = ifelse(FeatImpRank/10 < 1, paste0(0, FeatImpRank), paste0(FeatImpRank))
            functionOutput = singleCP(model = explainModel, explainIDs = explainIDs,  testMatrix = explainTestData,
                                      idx = explainIndex, method = method,
                                      column = column, length.out = 1000)
            
            ## saving output,  adjust foldername
            pathToSave = paste0(targetPath, dataSet,"/", method, "/singleCP")
            dir.create(pathToSave, showWarnings = F, recursive = T)
            ggsave(functionOutput$ggObj, filename = paste0(FeatImpRank, column, "_1.png"), device = "png",
                   path = pathToSave, width = 10, height = 7.5)
          }
        }
        
        if (multiple_ceterisParibus & type != "timeseries") {
          
          #------------ Multiple Ceteris Paribus plots-----------------------------------------------#
          for(column in TopImportantFeatures){
            print(paste0("multiple CP for " ,column))
            FeatImpRank = which(TopImportantFeatures == column)
            FeatImpRank = ifelse(FeatImpRank/10 < 1, paste0(0, FeatImpRank), paste0(FeatImpRank))
            functionOutput = multipleCP(model = explainModel, explainIDs = explainIDs,  testMatrix = explainTestData,
                                        idx = explainIDs[explainIndex],
                                        column = column, length.out = 100, method = method)
            
            ## saving output, adjust folder name
            pathToSave = paste0(targetPath, dataSet,"/", method, "/multipleCP")
            dir.create(pathToSave, showWarnings = F, recursive = T)
            # pngs
            ggsave(functionOutput$ggObj, filename = paste0(FeatImpRank, column, "_2.png"), device = "png",
                   path = pathToSave, width = 10, height = 7.5)
            # write JSON file
            readr::write_lines(jsonlite::toJSON(functionOutput$plotData, auto_unbox = T),paste0(pathToSave, "/", "multipleCP_", column, ".json"))
          }
        }
        
        ## -----------Dalex Plots -----------------------------------------------#
        
        if(waterfall & method == "xgBoost" &  type != "timeseries") {
          
          #setting matrixes for xgboost explainer
          explainTrainDataXgb = xgb.DMatrix(as.matrix(explainTrainData), 
                                            label = explainTrainLabel)
          explainTestDataXgb = xgb.DMatrix(as.matrix(explainTestData), 
                                           label = explainTestLabel)
          
          explainerXgb <- buildExplainer(xgb.model = explainModel,
                                         trainingData = explainTrainDataXgb,
                                         type = "regression",
                                         base_score = 0.5)
          pred.breakdown <- explainPredictions(xgb.model = explainModel,
                                               explainer = explainerXgb,
                                               data = explainTestDataXgb)
          
          # select all ids to explain. At the moment per default just Observation 1
          explainIDs2 = explainIndex
          
          for(explainID2 in explainIDs){
            ggObj = showWaterfall(xgb.model = explainModel,
                                  explainer = explainerXgb,
                                  DMatrix = explainTestDataXgb,
                                  data.matrix = as.matrix(explainTestData),
                                  idx = explainID2,
                                  type = "binary") +
              labs(title = paste("Dalexplot für Datenpunkt", explainID2),
                   subtitle = paste("Modell:", method)) 
           # + theme_wdl
              
            # saving png
            pathToSave = paste0(targetPath, dataSet,"/", method, "/dalex")
            dir.create(pathToSave, showWarnings = F, recursive = T)
            ggsave(ggObj, filename = paste0("dalex_id", explainID2, method , ".png"), device = "png",
                   path = pathToSave, width = 10, height = 7.5)
            
          }
        }
      }
    }
  }
}




#' @author Felix Kleine Bösing, modified folder structure by Marc Schmieder
#' @description evaluates modelling
#' @param dataPath directory in which the evaluation results are located
#' @param targetpat directory in which the evaluation results should be saved
#' @return Nothing. writes files as fst
saveEvaluationFiles <- function(dataPath = "03_computedData/06_evaluatedData/",
                                targetPath = "03_computedData/07_deploymentData/") {

  print("# Saving evaluation results")
  
  allFiles = list.files(dataPath)
  fileNames = allFiles[grep(allFiles, pattern = "filter")]

  dataSets = list()
  for(fileName in fileNames){
    dataSetName = gsub(fileName, pattern = ".rds", replacement = "")
    dataSets[[dataSetName]] = readRDS(paste0(dataPath, fileName))
  }

  unitedMeasures = uniteMeasures(dataSets)
  for (dataSetModel in names(unitedMeasures)) {
    for(modelN in names(unitedMeasures[[dataSetModel]])){
      for (measure in names(unitedMeasures[[dataSetModel]][[modelN]])){
        dataSetName = gsub(dataSetModel, pattern = "fold_", replacement = "")
        pathToSave = paste0(targetPath, dataSetName, "/", modelN, "/", measure)
        dir.create(pathToSave, showWarnings = F, recursive = T)
        if (class(unitedMeasures[[dataSetModel]][[modelN]][[measure]]) == "data.frame") {
          # write the data to a json file
          if (measure=="calculateAreaUnderCurve") {
            h=setDT(unitedMeasures[[dataSetModel]][[modelN]][[measure]])
            readr::write_lines(jsonlite::toJSON(h$Value, auto_unbox = T), paste0(pathToSave, "/", measure, ".json"))
          }else {
            readr::write_lines(jsonlite::toJSON(setDT(unitedMeasures[[dataSetModel]][[modelN]][[measure]])), paste0(pathToSave, "/", measure, ".json"))
          }
          # write a csv file
          write.csv(unitedMeasures[[dataSetModel]][[modelN]][[measure]], paste0(pathToSave,"/", measure, ".csv"))
        } else if (class(unitedMeasures[[dataSetModel]][[modelN]][[measure]]) == "list") {
          # # create new path with actual folder structur
          # dataSet = strsplit(dataSetModel, split = "_")[[1]][1]
          # method = strsplit(dataSetModel, split = "_")[[1]][2]
          # path = paste0(targetPath, "miv/", dataSet, "/", method, "/", measure)
          # dir.create(path, showWarnings = F, recursive = T)
          
          if (class(unitedMeasures[[dataSetModel]][[modelN]][[measure]][[paste(dataSetModel, modelN, sep = "_")]]) == "table") {
            # write JSON
            readr::write_lines(jsonlite::toJSON(as.data.table(unitedMeasures[[dataSetModel]][[modelN]][[measure]][[paste(dataSetModel, modelN, sep = "_")]])),
                        paste0(pathToSave, "/", measure, ".json"))
            write.table(unitedMeasures[[dataSetModel]][[modelN]][[measure]][[paste(dataSetModel, modelN, sep = "_")]],
                        paste0(pathToSave, "/", measure, ".txt"))
          } else if (any(class(unitedMeasures[[dataSetModel]][[modelN]][[measure]][[paste(dataSetModel, modelN, sep = "_")]]))
                     %in% c("ggplot", "gg")){
            png(paste0(pathToSave, "/", measure, ".png"))
            unitedMeasures[[dataSetModel]][[modelN]][[measure]][[paste(dataSetModel, modelN, sep = "_")]]
            dev.off()
          } else {
            png(paste0(pathToSave, "/", measure, ".png"))
            # dirty quick fix for missing auc title - move this into evaluation framework if project will be developed further
            # then add slot "title" to evaluationMeasures - Return
            if ("auc" %in% names(unitedMeasures[[dataSetModel]][[modelN]][[measure]][[paste(dataSetModel, modelN, sep = "_")]])) {
              test = plot(unitedMeasures[[dataSetModel]][[modelN]][[measure]][[paste(dataSetModel, modelN, sep = "_")]], 
                          main = paste0("AUC: ", round(unitedMeasures[[dataSetModel]][[modelN]][[measure]][[paste(dataSetModel, modelN, sep = "_")]]$auc, 3)))
            } else {
              plot(unitedMeasures[[measure]][[dataSetModel]])
            }
            dev.off()
          }
        }
      }
    }
  }
  
}


#' @author Felix Kleine Bösing
#' @description evaluates modelling
#' @param dataPath directory in which the evaluation results are located
#' @return Nothing. writes files as fst
uniteMeasures <- function(dataSets = list()) {
  measures = list()
  for (dataSetN in names(dataSets)) {
    dataSet = dataSets[[dataSetN]]
    for (modelN in names(dataSet)) {
      model = dataSet[[modelN]]
      for (measureN in names(model)) {
        measure = model[[measureN]]
        if (measure$type == "kpi") {
          if (measureN %in% names(measures)) {
            measures[[dataSetN]][[modelN]][[measureN]] = rbind(measures[[measureN]], data.frame(Measure = measureN, Model = modelN, 
                                                                          Value = measure$value, stringsAsFactors = F))
          } else {
            measures[[dataSetN]][[modelN]][[measureN]] = data.frame(Measure = measureN, Model = modelN, Value = measure$value, stringsAsFactors = F)
          }
        } else if (measure$type %in% c("plot", "table")) {
          elem = list()
          elem[[paste(dataSetN, modelN, sep = "_")]] = measure$value
          if (measureN %in% names(measures)) {
            measures[[dataSetN]][[modelN]][[measureN]] = c(measures[[measureN]], elem)
          } else {
            measures[[dataSetN]][[modelN]][[measureN]] = elem
          }
        }
      }
    }
  }
  return (measures)
}
