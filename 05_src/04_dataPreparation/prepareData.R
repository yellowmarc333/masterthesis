prepareDataBOW = function(inPath = "03_computedData/03_integratedData/", 
                          outPath = "03_computedData/04_preparedData/",
                          subsetSize = c("1pc", "10pc", "100pc", "Full"),
                          saveSparse = FALSE, mergeSD = FALSE){
  assertString(inPath)
  assertString(outPath)
  subsetSize <- match.arg(subsetSize)
  assertFlag(mergeSD)

  # readin files
  fileName <- paste0("trainSubset", subsetSize, ".fst")
  subsetData <- read.fst(path = paste0(inPath, fileName), 
                         as.data.table = TRUE)
  indexes <- readRDS(paste0(inPath, "indexes",
                            subsetSize, ".rds"))
  
  labelRaw <- as.factor(subsetData$category)
  
  if (mergeSD) {
    subsetData[, HeadLShortD := paste(headline, short_description, sep = ". ")]
    texts <- subsetData$HeadLShortD
  } else {
    texts <- subsetData$headline
  }
  
  tokens <- tokens(texts, what = "word", remove_numbers = FALSE, 
                   remove_punct = FALSE, remove_symbols = FALSE, 
                   remove_hyphens = FALSE)
  
  # remove stopwords
  tokens <- tokens_remove(tokens, c(stopwords("english")))
  
  # reduce to tokens > 0
  filter <- lengths(tokens) > 0
  # IndexDT for knowing which observations belong in train and test set
  IndexDT <- data.table(isFiltered = filter)[ , isTrainSet := .I %in% indexes]
  
  print(paste("removed tokes with 0 words. In total", sum(!filter)))
  
  # Create vocabulary. Terms will be unigrams (simple words).
  itoken <- text2vec::itoken(as.list(tokens), progressbar = FALSE)
  vocab <- text2vec::create_vocabulary(itoken)
  vocab <- text2vec::prune_vocabulary(vocab, term_count_min = 2L)
  vectorizer <- text2vec::vocab_vectorizer(vocab)
  
  labelRed <- as.factor(labelRaw)
  
  # create dtm
  tokens.sparse <- text2vec::create_dtm(
    itoken, vectorizer, skip_grams_window = 2L)
  
  if(!saveSparse) {
    tokens.dfm <- as.data.frame(as.matrix(tokens.sparse))
    tokens.dt <- as.data.table(tokens.dfm)
    rm(tokens.sparse)
    rm(tokens.dfm)
    
    result <- data.table(tokens.dt)
    rm(tokens.dt)
       
    resultTrain <- result[IndexDT$isFiltered & IndexDT$isTrainSet, ]
    resultTest <- result[IndexDT$isFiltered & !IndexDT$isTrainSet, ]
    assert(ncol(resultTrain) == ncol(resultTest))
    
    labelTrain <- labelRed[IndexDT$isFiltered & IndexDT$isTrainSet]
    labelTest <- labelRed[IndexDT$isFiltered & !IndexDT$isTrainSet]
    
    print(paste("check that train and test is rightly filtered: there are",
                - nrow(resultTrain) - nrow(resultTest) + nrow(subsetData),
                "deleted data points"))

    saveRDS(list(resultTrain = resultTrain,
                 resultTest = resultTest,
                 labelTrain = labelTrain,
                 labelTest = labelTest,
                 IndexDT = IndexDT), 
            file = paste0(outPath, "BOW-", 
                          subsetSize, "-", saveSparse,
                          "-", mergeSD, ".rds"),
            compress = FALSE)
    
  } else {
    result <- tokens.sparse
    rm(tokens.sparse)
    
    resultTrain <- result[IndexDT$isFiltered & IndexDT$isTrainSet, ]
    resultTest <- result[IndexDT$isFiltered & !IndexDT$isTrainSet, ]
    assert(ncol(resultTrain) == ncol(resultTest))
    
    labelTrain <- labelRed[IndexDT$isFiltered & IndexDT$isTrainSet]
    labelTest <- labelRed[IndexDT$isFiltered & !IndexDT$isTrainSet]
    
    print(paste("check that train and test is rightly filtered:",
                - nrow(resultTrain) - nrow(resultTest) + nrow(subsetData)))
    
    saveRDS(list(resultTrain = resultTrain,
                 resultTest = resultTest,
                 labelTrain = labelTrain,
                 labelTest = labelTest,
                 IndexDT = IndexDT), 
            file = paste0(outPath, "BOW-", 
                          subsetSize, "-", saveSparse,
                          "-", mergeSD, ".rds"),
            compress = FALSE)
  }

}




prepareDataTFIDF = function(inPath = "03_computedData/03_integratedData/", 
                            outPath = "03_computedData/04_preparedData/",
                            subsetSize = c("1pc", "10pc", "100pc", "Full"),
                            saveSparse = FALSE, mergeSD = FALSE){
  assertString(inPath)
  assertString(outPath)
  subsetSize <- match.arg(subsetSize)
  assertFlag(mergeSD)
  
  fileName <- paste0("trainSubset", subsetSize, ".fst")
  subsetData <- read.fst(path = paste0(inPath, fileName), 
                         as.data.table = TRUE)
  indexes <- readRDS(paste0(inPath, "indexes",
                            subsetSize, ".rds"))
  
  label <- subsetData$category
  labelRed <- as.factor(label)
  
  if (mergeSD) {
    subsetData[, HeadLShortD := paste(headline, short_description, sep = ". ")]
    texts <- subsetData$HeadLShortD
  } else {
    texts <- subsetData$headline
  }
  
  # Create iterator over tokens
  tokens <- tokens(texts, what = "word", remove_numbers = FALSE, 
                   remove_punct = FALSE, remove_symbols = FALSE, 
                   remove_hyphens = FALSE)
  
  # dont remove stopwords because are inducing meaning
  #tokens <- tokens_remove(tokens, c(stopwords("english")))
  
  # reduce to tokens > 0
  filter <- lengths(tokens) > 0
  
  # IndexDT for knowing which observations belong in train and test set
  IndexDT <- data.table(isFiltered = filter)[ , isTrainSet := .I %in% indexes]
  
  print(paste("removed tokes with 0 words. In total", sum(!filter)))
  
  tokens <- as.list(tokens)
  
  # Create vocabulary. Terms will be unigrams (simple words).
  itoken <- text2vec::itoken(tokens, progressbar = FALSE)
  vocab <- text2vec::create_vocabulary(itoken)
  vocab <- text2vec::prune_vocabulary(vocab, term_count_min = 2L)
  
  # Use our filtered vocabulary
  vectorizer <- text2vec::vocab_vectorizer(vocab)
  dtm = text2vec::create_dtm(itoken, vectorizer)
  dfm = as.dfm(dtm)
  
  tfidf = dfm_tfidf(x = dfm, scheme_tf = "augmented", 
                    scheme_df = "inverse", smoothing = 1,
                    k = 1)
  
  tokens.sparse <- tfidf
  
  if(!saveSparse) {
    tokens.dfm <- as.data.frame(as.matrix(tokens.sparse))
    tokens.dt <- as.data.table(tokens.dfm)
    tokens.dt[is.na(tokens.dt)] <- 0
    rm(tokens.sparse)
    rm(tokens.dfm)
    
    result <- data.table(tokens.dt)
    rm(tokens.dt)
    
    # todo hier ueberlegen ob das auch guenstiger geloest werden kann
    resultTrain <- result[IndexDT$isFiltered & IndexDT$isTrainSet, ]
    resultTest <- result[IndexDT$isFiltered & !IndexDT$isTrainSet, ]
    assert(ncol(resultTrain) == ncol(resultTest))
    
    labelTrain <- labelRed[IndexDT$isFiltered & IndexDT$isTrainSet]
    labelTest <- labelRed[IndexDT$isFiltered & !IndexDT$isTrainSet]
    
    print(paste("check that train and test is rightly filtered: there are",
                - nrow(resultTrain) - nrow(resultTest) + nrow(subsetData),
                "deleted data points"))
    
    saveRDS(list(resultTrain = resultTrain,
                 resultTest = resultTest,
                 labelTrain = labelTrain,
                 labelTest = labelTest,
                 IndexDT = IndexDT), 
            file = paste0(outPath, "TFIDF-", 
                          subsetSize, "-", saveSparse,
                          "-", mergeSD, ".rds"),
            compress = FALSE)
    
  } else {
    result <- tokens.sparse
    rm(tokens.sparse)
    
    resultTrain <- result[IndexDT$isFiltered & IndexDT$isTrainSet, ]
    resultTest <- result[IndexDT$isFiltered & !IndexDT$isTrainSet, ]
    assert(ncol(resultTrain) == ncol(resultTest))
    
    labelTrain <- labelRed[IndexDT$isFiltered & IndexDT$isTrainSet]
    labelTest <- labelRed[IndexDT$isFiltered & !IndexDT$isTrainSet]
    
    print(paste("check that train and test is rightly filtered:",
                - nrow(resultTrain) - nrow(resultTest) + nrow(subsetData)))
    
    saveRDS(list(resultTrain = resultTrain,
                 resultTest = resultTest,
                 labelTrain = labelTrain,
                 labelTest = labelTest,
                 IndexDT = IndexDT), 
            file = paste0(outPath, "TFIDF-", 
                          subsetSize, "-", saveSparse,
                          "-", mergeSD, ".rds"),
            compress = FALSE)
  }
  
}


prepareDataW2V = function(inPath = "03_computedData/03_integratedData/", 
                          outPath = "03_computedData/04_preparedData/",
                          subsetSize = c("1pc", "10pc", "100pc", "Full"),
                          word2VecSize = 50,
                          mergeSD = FALSE){
  assertString(inPath)
  assertString(outPath)
  subsetSize <- match.arg(subsetSize)
  assertNumber(word2VecSize)
  assertFlag(mergeSD)
  
  fileName <- paste0("trainSubset", subsetSize, ".fst")
  subsetData <- read.fst(path = paste0(inPath, fileName), 
                         as.data.table = TRUE)
  indexes <- readRDS(paste0(inPath, "indexes",
                            subsetSize, ".rds"))
   
  N <- nrow(subsetData)
  
  label <- subsetData$category
  labelRed <- as.factor(label)
  
  if (mergeSD) {
    subsetData[, HeadLShortD := paste(headline, short_description, sep = ". ")]
    
    texts <- subsetData$HeadLShortD
  } else {
    texts <- subsetData$headline
  }
  
  # Create iterator over tokens
  tokens <- quanteda::tokens(texts, what = "word", remove_numbers = FALSE, 
                             remove_punct = FALSE, remove_symbols = FALSE, 
                             remove_hyphens = FALSE)
  
  
  
  # dont remove stopwords because are inducing meaning
  #tokens <- tokens_remove(tokens, c(stopwords("english")))
  
  tokens <- as.list(tokens)
  
  # here select max words as smallest number of length, that at least
  # 0.999 of the Data points have (maybe complicated coded)
  tableWords <- sort(table(sapply(tokens, length)), decreasing = TRUE)
  cumsumWords <- cumsum(tableWords) / sum(tableWords)
  sortedNumWords <- sort(as.integer(names(which(cumsumWords > 0.999))), 
                         decreasing = FALSE)
  maxWords <- max(sortedNumWords[1:2])
  
  filter <- between(sapply(tokens, length), left = 0, right =  maxWords)
  # IndexDT for knowing which observations belong in train and test set
  IndexDT <- data.table(isFiltered = filter)[ , isTrainSet := .I %in% indexes]
  
  
  print(paste("removed tokes with 0 words. In total", sum(!filter)))
  
  # Create vocabulary. Terms will be unigrams (simple words).
  itoken <- text2vec::itoken(tokens, progressbar = FALSE)
  vocab <- text2vec::create_vocabulary(itoken)
  vocab <- text2vec::prune_vocabulary(vocab, term_count_min = 2L)
  
  # Use our filtered vocabulary
  vectorizer <- text2vec::vocab_vectorizer(vocab)
  # use window of 5 for context words
  tcm <- text2vec::create_tcm(itoken, vectorizer, skip_grams_window = 5L,
                              skip_grams_window_context = "symmetric")
  
  glove <- text2vec::GlobalVectors$new(word_vectors_size = word2VecSize, 
                                       vocabulary = vocab, x_max = 10, 
                                       #learning_rate = 0.1 * 100/word2VecSize,
                                       alpha = 0.75, lambda = 0)
  
  print("creating glove fit")
  glove$fit_transform(tcm, n_iter = 30)
  
  wordVectors <- as.data.table(glove$components)

  # write just the wordVectors
  write.fst(wordVectors, paste0(outPath, "W2VPure-", 
                                subsetSize, "-", word2VecSize,
                                "-", mergeSD, ".fst"),
            compress = 0)
  
  
  # testVector <- wordVectors[, trump]
  # 
  # cos_sim = sapply(data.table(wordVectors), function(x) {
  #   sum((x - testVector)^2)
  # })
  # cos_sim = text2vec::sim2(x = t(wordVectors), y = t(testVector),
  #                          method = "cosine",
  #                norm = "l2")
  # head(sort(cos_sim[,1], decreasing = TRUE), 20)
  
  wordVectorsNames <- colnames(wordVectors)
  channels <- nrow(wordVectors)
  
  print("creating wordVectorArray")
  wordVectorArray <- array(numeric(N* maxWords * channels),
                           dim = c(N, maxWords, channels))
  print("fill wordVectorArray")
  
  for(i in seq_along(tokens)) {
    
    if((i %% 1000) == 0) {
      print(paste("filling", i, "/", N))
    }
    
    tmpWords <- tokens[[i]]
    existWords <- tmpWords[tmpWords %in% wordVectorsNames]
    
    if(length(existWords) == 0){
      tmpMatrix <- matrix(numeric(channels* maxWords),
                          ncol = channels)
      
    } else {
      tmpMatrix <- as.matrix(t(wordVectors[, .SD, .SDcols = existWords]))
    }
    
    # fill up rows with 0's for equal array length, when length < maxWords
    if (nrow(tmpMatrix) < maxWords) {
      tmpFillUp <- rbind(tmpMatrix,
                         matrix(numeric((maxWords - nrow(tmpMatrix)) * 
                                          channels),
                                ncol = channels))
    } else if (nrow(tmpMatrix) > maxWords) {
      # other case for observations that are not deleted yet fill up with 0's
      tmpFillUp <- matrix(numeric(maxWords* channels), ncol = channels)
    } else { # if the observation has exactly maxWords
      tmpFillUp <- tmpMatrix
    }
    wordVectorArray[i, , ] <- tmpFillUp
  }
  
  print("finished filling")
  result <- wordVectorArray
  rm(wordVectorArray)
  
  resultTrain <- result[IndexDT$isFiltered & IndexDT$isTrainSet, ,]
  resultTest <- result[IndexDT$isFiltered & !IndexDT$isTrainSet, ,]
  assert(ncol(resultTrain) == ncol(resultTest))
  
  labelTrain <- labelRed[IndexDT$isFiltered & IndexDT$isTrainSet]
  labelTest <- labelRed[IndexDT$isFiltered & !IndexDT$isTrainSet]
  
  print(paste("check that train and test is rightly filtered:",
              - nrow(resultTrain) - nrow(resultTest) + nrow(subsetData)))
  
  saveRDS(list(resultTrain = resultTrain,
               resultTest = resultTest,
               labelTrain = labelTrain,
               labelTest = labelTest,
               IndexDT = IndexDT,
               maxWords = maxWords,
               channels = channels), 
          file = paste0(outPath, "W2VArray-", 
                        subsetSize, "-", word2VecSize,
                        "-", mergeSD, ".rds"),
          compress = FALSE)
  
  print("sum word vectors to text vectors")
  
  wordVectorSums <- vapply(tokens,FUN.VALUE = numeric(word2VecSize),
                           function(x, data = wordVectors) {
                             cols <- wordVectorsNames[wordVectorsNames %in% x]
                             res <- rowSums(data[, .SD, .SDcols = cols])
                             res <- res/sum(abs(res)) # L1 norm
                             if(length(res) == 0) return(rep(0, word2VecSize))
                             return(res)
                           })
  
  result <- data.table(t(wordVectorSums))
  
  resultTrain <- result[IndexDT$isFiltered & IndexDT$isTrainSet, ]
  resultTest <- result[IndexDT$isFiltered & !IndexDT$isTrainSet, ]
  assert(ncol(resultTrain) == ncol(resultTest))
  
  labelTrain <- labelRed[IndexDT$isFiltered & IndexDT$isTrainSet]
  labelTest <- labelRed[IndexDT$isFiltered & !IndexDT$isTrainSet]
  
  saveRDS(list(resultTrain = resultTrain,
               resultTest = resultTest,
               labelTrain = labelTrain,
               labelTest = labelTest,
               IndexDT = IndexDT), 
          file = paste0(outPath, "W2VSums-", 
                        subsetSize, "-", word2VecSize,
                        "-", mergeSD, ".rds"),
          compress = FALSE)
}

prepareDataSeq = function(inPath = "03_computedData/03_integratedData/", 
                          outPath = "03_computedData/04_preparedData/",
                          subsetSize = c("1pc", "10pc", "100pc", "Full"),
                          mergeSD = FALSE){
  
  assertString(inPath)
  assertString(outPath)
  subsetSize <- match.arg(subsetSize)
  assertFlag(mergeSD)
  
  fileName <- paste0("trainSubset", subsetSize, ".fst")
  subsetData <- read.fst(path = paste0(inPath, fileName), 
                         as.data.table = TRUE)
  indexes <- readRDS(paste0(inPath, "indexes",
                            subsetSize, ".rds"))
  
  N <- nrow(subsetData)
  
  label <- subsetData$category
  labelRed <- as.factor(label)
  
  if (mergeSD) {
    subsetData[, HeadLShortD := paste(headline, short_description, sep = ". ")]
    texts <- subsetData$HeadLShortD
  } else {
    texts <- subsetData$headline
  }
  
  # Create iterator over tokens
  tokens <- quanteda::tokens(texts, what = "word", remove_numbers = FALSE, 
                             remove_punct = FALSE, remove_symbols = FALSE, 
                             remove_hyphens = FALSE)
  
  
  # dont remove stopwords because are inducing meaning
  # tokens <- quanteda::tokens_remove(tokens, c(stopwords("english")))
  
  # prune vocabulary
  itoken <- text2vec::itoken(as.list(tokens), progressbar = FALSE)
  vocab <- as.data.table(text2vec::create_vocabulary(itoken))
  # select words which just occur 2 times or less
  rem_wordsvocab <- vocab[term_count <= 2 , term]

  tokens <- quanteda::tokens_remove(tokens, rem_wordsvocab)
  
  # here select max words as smallest number of length, that at least
  # 0.999 of the Data points have (maybe complicated coded)
  tableWords <- sort(table(sapply(tokens, length)), decreasing = TRUE)
  cumsumWords <- cumsum(tableWords) / sum(tableWords)
  sortedNumWords <- sort(as.integer(names(which(cumsumWords > 0.999))), 
                         decreasing = FALSE)
  maxWords <- max(sortedNumWords[1:2])
  
  filter <- between(sapply(tokens, length), left = 0, right =  maxWords)
  
  # IndexDT for knowing which observations belong in train and test set
  IndexDT <- data.table(isFiltered = filter)[ , isTrainSet := .I %in% indexes]

  print(paste("removed tokes with 0 words. In total", sum(!filter)))
  
  # bringt tokens back to text format for keras tokenizer
  tokens_text_list <- sapply(tokens, function(x) {
    if (length(x) == 0) return("")
    else return(do.call(paste, as.list(x)))
  })
  tokens_text <- unlist(tokens_text_list)
  
  # make integer sequences and pad them with keras
  myKerasTokenizer = keras::text_tokenizer() %>%
    keras::fit_text_tokenizer(x = tokens_text)
  sequences <- keras::texts_to_sequences(tokenizer = myKerasTokenizer, 
                                         texts = tokens_text)
  
  padded <- sequences %>%
    pad_sequences(maxlen = maxWords)
  
  result <- data.table(padded)
  rm(padded)
  
  resultTrain <- result[IndexDT$isFiltered & IndexDT$isTrainSet, ]
  resultTest <- result[IndexDT$isFiltered & !IndexDT$isTrainSet, ]
  assert(ncol(resultTrain) == ncol(resultTest))
  
  labelTrain <- labelRed[IndexDT$isFiltered & IndexDT$isTrainSet]
  labelTest <- labelRed[IndexDT$isFiltered & !IndexDT$isTrainSet]
  
  print(paste("check that train and test is rightly filtered: there are",
              - nrow(resultTrain) - nrow(resultTest) + nrow(subsetData),
              "deleted data points"))
  
  saveRDS(list(resultTrain = resultTrain,
               resultTest = resultTest,
               labelTrain = labelTrain,
               labelTest = labelTest,
               IndexDT = IndexDT), 
          file = paste0(outPath, "Seq-", 
                        subsetSize, "-",
                        mergeSD, ".rds"),
          compress = FALSE)
}



prepareDataGlove = function(inPath = "03_computedData/03_integratedData/", 
                            outPath = "03_computedData/04_preparedData/",
                            subsetSize = c("1pc", "10pc", "100pc", "Full"),
                            word2VecSize = 50,
                            mergeSD = FALSE,
                            gloveName = "glove.6B.50d.txt"){
  assertString(inPath)
  assertString(outPath)
  subsetSize <- match.arg(subsetSize)
  assertNumber(word2VecSize)
  assertFlag(mergeSD)
  
  fileName <- paste0("trainSubset", subsetSize, ".fst")
  subsetData <- read.fst(path = paste0(inPath, fileName), 
                         as.data.table = TRUE)
  indexes <- readRDS(paste0(inPath, "indexes",
                            subsetSize, ".rds"))
  N <- nrow(subsetData)
  
  label <- subsetData$category
  labelRed <- as.factor(label)
  
  if (mergeSD) {
    subsetData[, HeadLShortD := paste(headline, short_description, sep = ". ")]
    texts <- subsetData$HeadLShortD
  } else {
    texts <- subsetData$headline
  }
  
  # Create iterator over tokens
  tokens <- quanteda::tokens(texts, what = "word", remove_numbers = FALSE, 
                             remove_punct = FALSE, remove_symbols = FALSE, 
                             remove_hyphens = FALSE)
  
  tokens <- as.list(tokens)
  
  # dont remove stopwords because are inducing meaning
  #tokens <- tokens_remove(tokens, c(stopwords("english")))
  
  # here select max words as smallest number of length, that at least
  # 0.999 of the Data points have (maybe complicated coded)
  tableWords <- sort(table(sapply(tokens, length)), decreasing = TRUE)
  cumsumWords <- cumsum(tableWords) / sum(tableWords)
  sortedNumWords <- sort(as.integer(names(which(cumsumWords > 0.999))), 
                         decreasing = FALSE)
  maxWords <- max(sortedNumWords[1:2])
  
  filter <- between(sapply(tokens, length), left = 0, right =  maxWords)
  
  # IndexDT for knowing which observations belong in train and test set
  IndexDT <- data.table(isFiltered = filter)[ , isTrainSet := .I %in% indexes]
  
  print(paste("removed tokes with 0 words. In total", sum(!filter)))
  
  # Create vocabulary. Terms will be unigrams (simple words).
  itoken <- text2vec::itoken(tokens, progressbar = FALSE)
  vocab <- text2vec::create_vocabulary(itoken)
  vocab <- text2vec::prune_vocabulary(vocab, term_count_min = 2L)
  
  # Use our filtered vocabulary
  vectorizer <- text2vec::vocab_vectorizer(vocab)
  # use window of 5 for context words
   # insert glove here
  
  glove <- fread(paste0("02_initialData/", gloveName), quote = "")
  
  # see how many words are in glove of the data words
  notFound <- as.data.table(vocab[!(vocab$term %in% glove$V1),])
  commonWordsRatio <- round(mean(vocab$term %in% glove$V1), digits = 3)
  print(paste("the data words and Glove have", commonWordsRatio, 
              "in common"))
  print(paste(sum(notFound$doc_count), "words can not be assigned to wordvecs"))
  print(paste("from all", sum(sapply(tokens, length)), "words"))
  
  # reducing the vocab to the words that are not in glove
  vocabRest <- vocab[!(vocab$term %in% glove$V1), ]
  vocabGlove <- vocab[(vocab$term %in% glove$V1), ]
  
  wordVectorsRaw <- glove[V1 %in% vocabGlove$term, ]
  wordVectorsNames <- wordVectorsRaw$V1
  wordVectorsRaw[, V1 := NULL]
  wordVectors <- as.data.table(t(wordVectorsRaw))
  colnames(wordVectors) <- wordVectorsNames
  
  # write just the wordVectors
  write.fst(wordVectors, paste0(outPath, "GlovePure-", 
                                subsetSize, "-", word2VecSize,
                                "-", mergeSD, ".fst"),
            compress = 0)
  rm(wordVectorsRaw, glove)
  
  # testVector <- wordVectors[, dog]
  # 
  # cos_sim = sapply(data.table(wordVectors), function(x) {
  #   sum((x - testVector)^2)
  # })
  # cos_sim = text2vec::sim2(x = t(wordVectors), y = t(testVector),
  #                          method = "cosine",
  #                norm = "l2")
  # head(sort(cos_sim[,1], decreasing = TRUE), 20)
  N_tokens <- length(tokens)
  
  channels <- nrow(wordVectors)
  
  print("creating wordVectorArray")
  wordVectorArray <- array(numeric(N_tokens* maxWords * channels),
                           dim = c(N_tokens, maxWords, channels))
  print("fill wordVectorArray")
  
  for(i in seq_along(tokens)) {
    
    if((i %% 1000) == 0) {
      print(paste("filling", i, "/", N_tokens))
    }
    
    tmpWords <- tokens[[i]]
    existWords <- tmpWords[tmpWords %in% wordVectorsNames]
    
    if(length(existWords) == 0){
      tmpMatrix <- matrix(numeric(channels* maxWords),
                          ncol = channels)
      
    } else {
      tmpMatrix <- as.matrix(t(wordVectors[, .SD, .SDcols = existWords]))
    }
    
    # fill up rows with 0's for equal array length, when length < maxWords
    if (nrow(tmpMatrix) < maxWords) {
      tmpFillUp <- rbind(tmpMatrix,
                         matrix(numeric((maxWords - nrow(tmpMatrix)) * 
                                          channels),
                                ncol = channels))
    } else if (nrow(tmpMatrix) > maxWords) {
      # other case for observations that are not deleted yet fill up with 0's
      tmpFillUp <- matrix(numeric(maxWords* channels), ncol = channels)
    } else { # if the observation has exactly maxWords
      tmpFillUp <- tmpMatrix
    }
    wordVectorArray[i, , ] <- tmpFillUp
  }
  
  print("finished filling")
  result <- wordVectorArray
  rm(wordVectorArray)
  
  resultTrain <- result[IndexDT$isFiltered & IndexDT$isTrainSet, ,]
  resultTest <- result[IndexDT$isFiltered & !IndexDT$isTrainSet, ,]
  assert(ncol(resultTrain) == ncol(resultTest))
  
  labelTrain <- labelRed[IndexDT$isFiltered & IndexDT$isTrainSet]
  labelTest <- labelRed[IndexDT$isFiltered & !IndexDT$isTrainSet]
  
  print(paste("check that train and test is rightly filtered:",
              - nrow(resultTrain) - nrow(resultTest) + nrow(subsetData)))
  
  saveRDS(list(resultTrain = resultTrain,
               resultTest = resultTest,
               labelTrain = labelTrain,
               labelTest = labelTest,
               IndexDT = IndexDT,
               maxWords = maxWords,
               channels = channels), 
          file = paste0(outPath, "GloveArray-", 
                        subsetSize, "-", word2VecSize,
                        "-", mergeSD, ".rds"),
          compress = FALSE)
  
  rm(result, resultTrain, resultTest, labelTrain, labelTest)
  
  print("sum word vectors to text vectors")
  
  wordVectorSums <- vapply(tokens,FUN.VALUE = numeric(word2VecSize),
                           function(x, data = wordVectors) {
                             cols <- wordVectorsNames[wordVectorsNames %in% x]
                             res <- rowSums(data[, .SD, .SDcols = cols])
                             res <- res/sum(abs(res)) # L1 norm
                             if(length(res) == 0) return(rep(0, word2VecSize))
                             return(res)
                           })
  
  result <- data.table(t(wordVectorSums))
  
  resultTrain <- result[IndexDT$isFiltered & IndexDT$isTrainSet, ]
  resultTest <- result[IndexDT$isFiltered & !IndexDT$isTrainSet, ]
  assert(ncol(resultTrain) == ncol(resultTest))
  
  labelTrain <- labelRed[IndexDT$isFiltered & IndexDT$isTrainSet]
  labelTest <- labelRed[IndexDT$isFiltered & !IndexDT$isTrainSet]
  
  saveRDS(list(resultTrain = resultTrain,
               resultTest = resultTest,
               labelTrain = labelTrain,
               labelTest = labelTest,
               IndexDT = IndexDT), 
          file = paste0(outPath, "GloveSums-", 
                        subsetSize, "-", word2VecSize,
                        "-", mergeSD, ".rds"),
          compress = FALSE)
}


pipelineEmbBinary = function(inPath = "03_computedData/03_integratedData/", 
                             outPath = "03_computedData/04_preparedData/",
                             subsetSize = c("1pc", "10pc", "100pc", "Full"),
                             mergeSD = FALSE, 
                             binary = FALSE){
  assertString(inPath)
  assertString(outPath)
  subsetSize <- match.arg(subsetSize)
  assertFlag(mergeSD)
  
  fileName <- paste0("trainSubset", subsetSize, ".fst")
  wholeData <- read.fst(path = paste0(inPath, fileName), 
                        as.data.table = TRUE)
  
  categories <- unique(wholeData$category)
  
  categoryPairs <- matrix(c("EDUCATION", "COLLEGE",
                            "ARTS & CULTURE", "ARTS",
                            "STYLE", "STYLE & BEAUTY",
                            "CULTURE & ARTS", "ARTS & CULTURE",
                            "CULTURE & ARTS", "ARTS",
                            "PARENTS", "PARENTING",
                            "DIVORCE", "WEDDINGS",
                            "WELLNESS", "HEALTHY LIVING",
                            "GREEN", "HEALTHY LIVING", 
                            "GREEN", "ENVIRONMENT"), ncol = 2, byrow = TRUE)
  
  result <- data.table(categoryPairs, accuracy = 0)
  
  for (row in seq_len(nrow(categoryPairs))){
    print(paste("calculating row", paste(categoryPairs[row, ])))
    
    # subsetData <- wholeData[category %in% c("WORLD NEWS", "MEDIA"),]
    subsetData <- wholeData[category %in% categoryPairs[row ,],]
    
    N <- nrow(subsetData)
    label <- subsetData$category
    texts <- subsetData$headline
    
    # Create iterator over tokens
    tokens <- quanteda::tokens(texts, what = "word", remove_numbers = FALSE, 
                               remove_punct = TRUE, remove_symbols = TRUE, 
                               remove_hyphens = FALSE)
    
    
    
    # dont remove stopwords because are inducing meaning
    tokens <- quanteda::tokens_remove(tokens, c(stopwords("english")))
    
    # dont use wordstemming
    #tokens <- tokens_wordstem(tokens, language = "english")
    
    # prune vocabulary
    itoken <- text2vec::itoken(as.list(tokens), progressbar = FALSE)
    vocab <- as.data.table(text2vec::create_vocabulary(itoken))
    # select words which just occur 2 times or less
    rem_wordsvocab <- vocab[term_count <= 2 , term]
    tokens <- quanteda::tokens_remove(tokens, rem_wordsvocab)
    
    # here select max words as smallest number of length, that at least
    # 0.999 of the Data points have (maybe complicated coded)
    tableWords <- sort(table(sapply(tokens, length)), decreasing = TRUE)
    cumsumWords <- cumsum(tableWords) / sum(tableWords)
    sortedNumWords <- sort(as.integer(names(which(cumsumWords > 0.999))), 
                           decreasing = FALSE)
    maxWords <- max(sortedNumWords[1:2], na.rm = TRUE)
    
    tokens <- tokens[sapply(tokens, length) <= maxWords]
    
    # bringt tokens back to text format for keras tokenizer
    tokens_text_list <- sapply(tokens, function(x) {
      do.call(paste, as.list(x))
    })
    tokens_text <- unlist(tokens_text_list)
    
    # make integer sequences and pad them with keras
    myKerasTokenizer = keras::text_tokenizer() %>%
      keras::fit_text_tokenizer(x = tokens_text)
    sequences <- keras::texts_to_sequences(tokenizer = myKerasTokenizer, 
                                           texts = tokens_text)
    
    padded <- sequences %>%
      pad_sequences(maxlen = maxWords)
    
    labelIndexes <- as.integer(gsub(names(tokens_text), 
                                    pattern = "text", 
                                    replacement = ""))
    
    labelRed <- label[labelIndexes]
    
    
    # here everything for the emb CNN is prepared
    data <- data.table(labelRaw = as.factor(labelRed), padded)
    
    label <- data$labelRaw
    
    indexes <- sample.int(nrow(data), size = round(nrow(data)* 0.9))
    
    trainData <- data[indexes,]
    testData <- data[-indexes, ]
    
    trainData[, labelRaw := NULL]
    testData[, labelRaw := NULL]
    
    trainLabelRaw <- label[indexes]
    testLabelRaw <- label[-indexes]
    
    if ( (length(unique(trainLabelRaw)) != 2) |
         (length(unique(testLabelRaw)) != 2) ) next
    
    trainLabel <- to_categorical(as.numeric(label[indexes]) - 1)
    testLabel <- to_categorical(as.numeric(label[-indexes]) - 1)
    
    nVocab = max(rbind(trainData,testData)) + 1
    
    model <- keras_model_sequential() %>% 
      # Start off with an efficient embedding layer which maps
      # the vocab indices into embedding_dims dimensions
      layer_embedding(input_dim = nVocab,
                      output_dim = 100, 
                      input_length = ncol(trainData)) %>%
      layer_dropout(0.2) %>%
      
      # Add a Convolution1D, which will learn filters
      layer_conv_1d(filters = 100, kernel_size  = 2, 
                    padding = "valid", activation = "relu", strides = 1
      ) %>%
      layer_dropout(0.2) %>%
      layer_conv_1d(filters = 100, kernel_size = 3,
                    padding = "same", activation = "relu",
                    strides = 1,
                    name = "conv2") %>%
      layer_conv_1d(filters = 100, kernel_size = 4,
                    padding = "same", activation = "relu",
                    strides = 1,
                    name = "conv3") %>%
      layer_dropout(0.2) %>%
      layer_conv_1d(filters = 100, kernel_size = 5,
                    padding = "same", activation = "relu",
                    strides = 1,
                    name = "conv4") %>%
      # Apply max pooling:
      layer_dropout(0.2) %>%
      layer_global_max_pooling_1d() %>%
      
      # Add a vanilla hidden layer:
      layer_dense(units = 100) %>%
      # Add a vanilla hidden layer:
      layer_dense(units = 50) %>%
      
      # Apply 20% layer dropout
      layer_dropout(0.2) %>%
      layer_activation("relu") %>%
      
      # Project onto a single unit output layer, and squash it with a sigmoid
      layer_dense(units = ncol(trainLabel),
                  activation = "softmax", 
                  name = "predictions")
    
    
    # Compiling model
    model %>% compile(
      loss = 'binary_crossentropy',
      optimizer = optimizer_rmsprop(lr = 0.001),
      metrics = c('accuracy')
    )
    
    print(paste("fitting model", row))
    
    history <- model %>% fit(
      x = as.matrix(trainData),
      y = trainLabel,
      epochs = 5,
      batchsize = 32,
      validation_data = list(as.matrix(testData), testLabel),
      view_metrics = FALSE,
      verbose = 2)
    
    
    predictionResult <- model %>% 
      evaluate(as.matrix(testData), testLabel, batch_size = 32)
    
    result[row, accuracy := predictionResult$acc]
  }
  
  return(result)
}
