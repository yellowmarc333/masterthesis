

#----------------------- 10 percent models ---------------------------------####

# naming is mod_Wordembedding_Algorithm_Subsetsize
predictPreselection <- function() {
  dataPath <- "03_computedData/04_preparedData/"
  psPath <- "03_computedData/05_modelData/preselection/"

  dir.create("03_computedData/05_modelData/preselection/", recursive = TRUE,
             showWarnings = FALSE)
  dateNow <- Sys.Date()
  
  
  
  # TFIDF ####
  calc <- function() {
    fileName <- "TFIDF-10pc-TRUE-FALSE.rds"
    modelRes <- predictXG(dataPath, fileName,
                          sparse = TRUE, nrounds = 250) #0.455
    modelName <- "mod_TFIDF_XG_10"
    saveRDS(modelRes,
            file = paste0(psPath, modelName, "_", dateNow, ".RDS"))
  }
  try(calc())
  
  calc <- function() {
    fileName <- "TFIDF-10pc-TRUE-FALSE.rds"
    modelRes <- predictRF(dataPath, fileName, 
                          sparse = TRUE, num.trees = 50) # 0.443 (3k trees tried)
    modelName <- "mod_TFIDF_RF_10"
    
    saveRDS(modelRes,
            file = paste0(psPath, modelName, "_", dateNow, ".RDS"))
  }
  try(calc())
  
  
  calc <- function() {
    fileName <- "TFIDF-10pc-FALSE-FALSE.rds"
    modelRes <- predictMLP(dataPath, fileName, epochs = 15)
    modelName <- "mod_TFIDF_MLP_10"
    # 0.5159 std
    # mit aktivations: 0.499
    # mit batch überall: 0.4974
    # komplett ohne batch: 0.5120
    saveRDS(modelRes,
            file = paste0(psPath, modelName, "_", dateNow, ".RDS"))
  }
  try(calc())
  # now tuning
  # 1: std 0.4946
  # 2: with activation = relu everywhere: 0.4703
  # 3: 1 with batch norm 48,88
  # 4: 1 with dropout 0.3 0.4816
  # 5: 1 with learning rate = 0.0001: 0.4791
  # 6: 1 with optimizer rmsprop, 0.4924
  
  
  
  # BOW ####
  calc <- function() {
    fileName <- "BOW-10pc-TRUE-FALSE.rds"
    modelRes <- predictXG(dataPath, fileName,
                          sparse = TRUE, nrounds = 250)
    modelName <- "mod_BOW_XG_10"
    
    saveRDS(modelRes,
            file = paste0(psPath, modelName, "_", dateNow, ".RDS"))
  }
  try(calc())
  
  calc <- function() {
    fileName <- "BOW-10pc-TRUE-FALSE.rds"
    modelRes <- predictRF(dataPath, fileName, 
                          sparse = TRUE, num.trees = 500)
    modelName <- "mod_BOW_RF_10"
    
    saveRDS(modelRes,
            file = paste0(psPath, modelName, "_", dateNow, ".RDS"))
  }
  try(calc())
  
  
  calc <- function() {
    fileName <- "BOW-10pc-FALSE-FALSE.rds"
    modelRes <- predictMLP(dataPath, fileName,
                           epochs = 15)
    modelName <- "mod_BOW_MLP_10"
    
    saveRDS(modelRes,
            file = paste0(psPath, modelName, "_", dateNow, ".RDS"))
  }
  try(calc())
  
  
  ## sequences ####
  
  calc <- function() {
    # CNN Seq 10pc #28-11
    fileName <- "Seq-10pc-FALSE.rds"
    modelRes <- predictCNNSeq(dataPath, fileName, 
                              epochs = 15)
    # wie gehabt:  0.4451
    # dropout am anfang weg, +5kernel: 0.4299
    # dropout am anfang wieder hin: 0.39
    # dropout weg, 5kernel weg: 0:445
    # dropout weg, 4kernel weg: 
    # mit 300d emb:
    modelName <- "mod_Seq_CNNSeq_10"
    
    saveRDS(modelRes,
            file = paste0(psPath, modelName, "_", dateNow, ".RDS"))
  }
  try(calc())
  
  
  calc <- function() {
    fileName <- "Seq-10pc-FALSE.rds"
    modelRes <- predictLSTMSeq(dataPath, fileName, 
                               epochs = 15) 
    # normal: 0.4891
    # mit relu vor dropout am ende, lr = 0.0005: 0.4612
    # mit 300d
    modelName <- "mod_Seq_LSTMSeq_10"
    
    saveRDS(modelRes,
            file = paste0(psPath, modelName, "_", dateNow, ".RDS"))
  }
  try(calc())
  
  
  
  # wordvectorSums ####
  
  calc <- function() {
    fileName <- "W2VSums-10pc-50-FALSE.rds"
    modelRes <- predictMLP(dataPath, fileName, epochs = 15)
    modelName <- "mod_W2VSum50_NN_10"
    
    saveRDS(modelRes,
            file = paste0(psPath, modelName, "_", dateNow, ".RDS"))
  }
  try(calc())
  
  
  calc <- function() {
    fileName <- "W2VSums-10pc-50-FALSE.rds"
    modelRes <- predictXG(dataPath, fileName, nrounds = 250)
    modelName <- "mod_W2VSum50_XG_10"
    
    saveRDS(modelRes,
            file = paste0(psPath, modelName, "_", dateNow, ".RDS"))
  }
  try(calc())
  
  calc <- function() {
    fileName <- "W2VSums-10pc-50-FALSE.rds"
    modelRes <- predictRF(dataPath, fileName, num.trees = 500)
    modelName <- "mod_W2VSum50_RF_10"
    
    saveRDS(modelRes,
            file = paste0(psPath, modelName, "_", dateNow, ".RDS"))
  }
  try(calc())
  
  
  
  calc <- function() {
    fileName <- "GloveSums-10pc-50-FALSE.rds"
    modelRes <- predictMLP(dataPath, fileName, epochs = 15)
    modelName <- "mod_GloveSum50_NN_10"
    
    saveRDS(modelRes,
            file = paste0(psPath, modelName, "_", dateNow, ".RDS"))
  }
  try(calc())
  
  calc <- function() {
    fileName <- "GloveSums-10pc-50-FALSE.rds"
    modelRes <- predictXG(dataPath, fileName, nrounds = 250)
    modelName <- "mod_GloveSum50_XG_10"
    
    saveRDS(modelRes,
            file = paste0(psPath, modelName, "_", dateNow, ".RDS"))
  }
  try(calc())
  
  calc <- function() {
    fileName <- "GloveSums-10pc-50-FALSE.rds"
    modelRes <- predictRF(dataPath, fileName, num.trees = 500)
    modelName <- "mod_GloveSum50_RF_10"
    
    saveRDS(modelRes,
            file = paste0(psPath, modelName, "_", dateNow, ".RDS"))
  }
  try(calc())
  
  
  calc <- function() {
    fileName <- "GloveSums-10pc-300-FALSE.rds"
    modelRes <- predictMLP(dataPath, fileName, epochs = 15)
    modelName <- "mod_GloveSum300_NN_10"
    
    saveRDS(modelRes,
            file = paste0(psPath, modelName, "_", dateNow, ".RDS"))
  }
  try(calc())
  
  calc <- function() {
    fileName <- "GloveSums-10pc-300-FALSE.rds"
    modelRes <- predictXG(dataPath, fileName, nrounds = 250) # 0.4747
    modelName <- "mod_GloveSum300_XG_10"
    
    saveRDS(modelRes,
            file = paste0(psPath, modelName, "_", dateNow, ".RDS"))
  }
  try(calc())

  
  calc <- function() {
    fileName <- "GloveSums-10pc-300-FALSE.rds"
    modelRes <- predictRF(dataPath, fileName, num.trees = 500)
    modelName <- "mod_GloveSum300_RF_10"
    
    saveRDS(modelRes,
            file = paste0(psPath, modelName, "_", dateNow, ".RDS"))
  }
  try(calc())
  
  
  # glove Arrays ####
  
  calc <- function() {
    fileName <- "W2VArray-10pc-50-FALSE.rds"
    modelRes <- predictCNNArray(dataPath, fileName, 
                                epochs = 15) # 0.3315
    modelName <- "mod_W2VArray_CNNArray_10"
    
    saveRDS(modelRes,
            file = paste0(psPath, modelName, "_", dateNow, ".RDS"))
  }
  try(calc())
  
  calc <- function() {
    fileName <- "W2VArray-10pc-50-FALSE.rds"
    modelRes <- predictLSTMArray(dataPath, fileName, 
                                 epochs = 15) # 0.3315
    modelName <- "mod_W2VArray_LSTMArray_10"
    
    saveRDS(modelRes,
            file = paste0(psPath, modelName, "_", dateNow, ".RDS"))
  }
  try(calc())
  
  calc <- function() {
    fileName <- "GloveArray-10pc-50-FALSE.rds"
    modelRes <- predictCNNArray(dataPath, fileName = fileName, 
                                epochs = 15) 
    # normale struktur: 52,53
    # ohne batch: 0.5134
    # mit dropout 0.2: 0.5294
    modelName <- "mod_GloveArray50_CNNArray_10"
    
    saveRDS(modelRes,
            file = paste0(psPath, modelName, "_", dateNow, ".RDS"))
  }
  try(calc())
  
  
  calc <- function() {
    fileName <- "GloveArray-10pc-50-FALSE.rds"
    modelRes <- predictLSTMArray(dataPath, 
                                 fileName = fileName, 
                                 epochs = 15) 
    # normal: 0.5369
    modelName <- "mod_GloveArray50_LSTMArray_10"
    
    saveRDS(modelRes,
            file = paste0(psPath, modelName, "_", dateNow, ".RDS"))
  }
  try(calc())
  
  calc <- function() {
    fileName <- "GloveArray-10pc-300-FALSE.rds"
    modelRes <- predictCNNArray(dataPath, fileName = fileName, 
                                epochs = 15)
    # 1)tuning: normal 0.5264, 7 am besten mit 0.5355 (epoche 4)
    # 2, 1 mit 2x 50 vanilla layer : 0.5212
    # 3, 1 ohne vanillas: 0.5330
    # 4, 3 mit average pooling: und flatten: 0.5106
    # 5, 3 mit filter size 128, 64, 32, 16: 0.5001
    # 6, 3 mit filter size umgekehrt 128, 64, 32, 16: 0.5106
    # 7, 3 mit 100, 100, 100, 100 0.5317
    # 8, 7 mit kernels 2, 3, 4, 5, 0.5355
    # 9, 8 + neues layer mit 6 kernels 0.5129
    modelName <- "mod_GloveArray300_CNNArray_10"
    
    saveRDS(modelRes,
            file = paste0(psPath, modelName, "_", dateNow, ".RDS"))
  }
  try(calc())
  
  calc <- function() {
    fileName <- "GloveArray-10pc-300-FALSE.rds"
    modelRes <- predictLSTMArray(dataPath, 
                                 fileName = fileName, 
                                 epochs = 15) # 0.6011 
    # ohne bidirectional 0.5919
    
    # tuning:
    # 1: ohne densenet 0.5449
    # 2: 1 ohne conv net 0.5701
    # 3: 2 mit 256 units 0.5657
    # 4: 2 mit nadam 0.5706
    
    # gru: 0.61
    modelName <- "mod_GloveArray300_LSTMArray_10"
    
    saveRDS(modelRes,
            file = paste0(psPath, modelName, "_", dateNow, ".RDS"))
  }
  try(calc())
  
}

#----------------------- 10 pc to analyse---------------------####

calc <- function() {
  fileName <- "GloveArray-10pc-300-FALSE.rds"
  modelRes <- predictCNNArray(dataPath, fileName = fileName, 
                              epochs = 4)
  modelName <- "mod_GloveArray300_CNNArray_10New"
  
  saveRDS(modelRes,
          file = paste0("03_computedData/05_modelData/finalselection/", 
                        modelName, "_", ".RDS"))
}




#----------------------- 100 percent models --------------------------------####

predictFullModels <- function() {
  dataPath <- "03_computedData/04_preparedData/"
  psPath <- "03_computedData/05_modelData/finalModels/"
  
  dir.create("03_computedData/05_modelData/finalModels/", recursive = TRUE,
             showWarnings = FALSE)
  dateNow <- Sys.Date()
  
  
  calc <- function() {
    fileName <- "BOW-Full-TRUE-FALSE.rds"
    modelRes <- predictXG(dataPath, fileName,
                          sparse = TRUE, nrounds = 250)
    modelName <- "mod_BOW_XG_Full"
    
    saveRDS(modelRes,
            file = paste0(psPath, modelName, "_", dateNow, ".RDS"))
  }
  try(calc())
  gc()
  
  calc <- function() {
    fileName <- "GloveSums-Full-300-FALSE.rds"
    modelRes <- predictXG(dataPath, fileName,
                          sparse = FALSE, nrounds = 250)
    modelName <- "mod_GloveSums300_XG_Full"
    
    saveRDS(modelRes,
            file = paste0(psPath, modelName, "_", dateNow, ".RDS"))
  }
  try(calc())
  gc()
  
  # relevant atm
  calc <- function() {
    fileName <- "GloveSums-Full-300-FALSE.rds"
    modelRes <- predictMLP(dataPath, fileName,
                          epochs = 30)
    modelName <- "mod_GloveSums300_MLP_Full"
    
    saveRDS(modelRes,
            file = paste0(psPath, modelName, "_", dateNow, ".RDS"))
  }
  try(calc())
  gc()
  
 
  calc <- function() {
    fileName <- "GloveArray-Full-300-FALSE.rds"
    modelRes <- predictCNNArray(dataPath, fileName,
                                epochs = 30) 
    modelName <- "mod_GloveArray300_CNNArray_Full"
    
    saveRDS(modelRes,
            file = paste0(psPath, modelName, "_", dateNow, ".RDS"))
  }
  try(calc())
  gc()
  
  calc <- function() {
    fileName <- "GloveArray-Full-300-FALSE.rds"
    modelRes <-  predictLSTMArray(dataPath, fileName = fileName, 
                                  epochs = 30) 
    modelName <- "mod_GloveArray300_LSTMArray_Full"
    
    saveRDS(modelRes,
            file = paste0(psPath, modelName, "_", dateNow, ".RDS"))
  }
  try(calc())
  gc()
  
  
}


#-------------------------- Full predictions ---------------####

# # BOW full # 03.01
# fileName <- "BOW-Full-TRUE-FALSE.rds"
# mod_BOW_XG_10 <- predictXG(dataPath, fileName,
#                            sparse = TRUE, nrounds = 100) # 0.5968
# # funktioniert


  #--------------------------- Ensembling ------------------------------------####

# accComparison <- data.table(category = names(mod_SeqCNN10$accByClass),
#                             SeqCNN10 = mod_SeqCNN10$accByClass,
#                             SeqLSTM10 = mod_SeqLSTM10$accByClass,
#                             CNNGlove10 =  mod_CNNGlove10$accByClass,
#                             LSTMArray10 = mod_LSTMArray10$accByClass, 
#                             RF10 = mod_RF10$accByClass)
# View(accComparison)
# write.rds(accComparison, 
#           path = "03_computedData/06_evaluatedData/accComparison.rds",
#           compress = 0)
# 
# ensSeqeRes1 <- ensSeqleMaxProb(mod_XG2$predictions, 
#                                mod_RF2$predictions,
#                                mod_Seq$predictions,
#                                truth = mod_XG2$testLabelRaw)
# 
# 


