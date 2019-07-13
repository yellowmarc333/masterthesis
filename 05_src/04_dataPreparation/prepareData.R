prepareDataBOW = function(inPath = "03_computedData/02_cleanedData/", 
                       outPath = "03_computedData/04_preparedData/",
                       subsetSize = c("1pc", "10pc", "100pc"),
                       mergeSD = FALSE){
  assertString(inPath)
  assertString(outPath)
  subsetSize <- match.arg(subsetSize)
  assertFlag(mergeSD)
  
  list.files(path = inPath)
  subsetData <- read.fst(path = paste0(inPath, "trainSubset10pc.fst"), 
                         as.data.table = TRUE)
  
  label <- oneHotEncode(subsetData$category)
  labelRaw <- as.factor(subsetData$category)
  subsetData[, HeadLShortD := paste(headline, short_description, sep = ". ")]
  if (mergeSD) {
    texts <- subsetData$HeadLShortD
  } else {
    texts <- subsetData$headline
  }
  
  tokens <- tokens(texts, what = "word", remove_numbers = FALSE, 
                   remove_punct = FALSE, remove_symbols = FALSE, 
                   remove_hyphens = TRUE)
  # lower all cases
  tokens <- tokens_tolower(tokens)
  
  # remove stopwords
  tokens <- tokens_remove(tokens, c(stopwords("english")))
  
  # wordstemming
  #tokens <- tokens_wordstem(tokens, language = "english")
  
  # reduce to tokens > 0
  hasWords <- lengths(tokens) > 0
  tokens <- tokens[hasWords]
  label = label[hasWords]
  labelRaw = labelRaw[hasWords]
  

  tokens.dfm <- dfm(tokens)
  
  tokens.dfm <- quanteda::dfm_trim(tokens.dfm, min_docfreq = 2)
  tokens.dt <- as.data.table(quanteda::convert(tokens.dfm, to = "data.frame"))
  tokens.dt[, document := NULL]
  tokens.dt.label <- data.table(labelRaw, tokens.dt)
  
  write.fst(tokens.dt.label, path = paste0(outPath, "BOW-", 
                                           subsetSize, "-", mergeSD, ".fst"))
}



prepareDataTFIDF = function(inPath = "03_computedData/03_integratedData/", 
                          outPath = "03_computedData/04_preparedData/",
                          subsetSize = c("1pc", "10pc", "100pc"),
                          mergeSD = FALSE){
  assertString(inPath)
  assertString(outPath)
  subsetSize <- match.arg(subsetSize)
  assertFlag(mergeSD)
  

  
  fileName <- paste0("trainSubset", subsetSize, ".fst")
  subsetData <- read.fst(path = paste0(inPath, fileName), 
                   as.data.table = TRUE)

  label <- subsetData$category
  
  subsetData[, HeadLShortD := paste(headline, short_description, sep = ". ")]
  if (mergeSD) {
    texts <- subsetData$HeadLShortD
  } else {
    texts <- subsetData$headline
  }

  # Create iterator over tokens
  tokens <- quanteda::tokens(texts, what = "word", remove_numbers = FALSE, 
                              remove_punct = FALSE, remove_symbols = FALSE, 
                              remove_hyphens = TRUE)
  
  # lower all cases
  tokens <- quanteda::tokens_tolower(tokens)
  
  # remove stopwords because are inducing meaning
  tokens <- tokens_remove(tokens, c(stopwords("english")))
  
  # use wordstemming
  #tokens <- tokens_wordstem(tokens, language = "english")
  
  tokens <- as.list(tokens)
  
  # Create vocabulary. Terms will be unigrams (simple words).
  itoken <- text2vec::itoken(tokens, progressbar = FALSE)
  vocab <- text2vec::create_vocabulary(itoken)
  vocab <- text2vec::prune_vocabulary(vocab, term_count_min = 2L)
  
  # Use our filtered vocabulary
  vectorizer <- text2vec::vocab_vectorizer(vocab)
  dtm = text2vec::create_dtm(itoken, vectorizer)

  # define tfidf model
  tfidf = TfIdf$new()
  tfidf_fit = tfidf$fit_transform(x = dtm, tfidf)
  # fit model to train data and transform train data with fitted model
  tfidf_data <- data.table(labelRaw = as.factor(label), as.matrix(tfidf_fit))

  write.fst(tfidf_data, paste0(outPath, "TFIDF-", 
                                subsetSize, "-",
                                "-", mergeSD, ".fst"))
  
}


prepareDataW2V = function(inPath = "03_computedData/03_integratedData/", 
                          outPath = "03_computedData/04_preparedData/",
                          subsetSize = c("1pc", "10pc", "100pc"),
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
  
  N <- nrow(subsetData)
  
  label <- subsetData$category
  
  subsetData[, HeadLShortD := paste(headline, short_description, sep = ". ")]
  if (mergeSD) {
    texts <- subsetData$HeadLShortD
  } else {
    texts <- subsetData$headline
  }
  
  # Create iterator over tokens
  tokens <- quanteda::tokens(texts, what = "word", remove_numbers = FALSE, 
                             remove_punct = TRUE, remove_symbols = TRUE, 
                             remove_hyphens = TRUE)
  
  # lower all cases
  tokens <- quanteda::tokens_tolower(tokens)
  
  # dont remove stopwords because are inducing meaning
  tokens <- tokens_remove(tokens, c(stopwords("english")))
  
  # dont use wordstemming
  #tokens <- tokens_wordstem(tokens, language = "english")
  
  tokens <- as.list(tokens)
  
  # here select max words as smallest number of length, that at least
  # 0.999 of the Data points have (maybe complicated coded)
  tableWords <- sort(table(sapply(tokens, length)), decreasing = TRUE)
  cumsumWords <- cumsum(tableWords) / sum(tableWords)
  sortedNumWords <- sort(as.integer(names(which(cumsumWords > 0.999))), 
                         decreasing = FALSE)
  maxWords <- max(sortedNumWords[1:2])
  
  tokens <- tokens[sapply(tokens, length) <= maxWords]
  
  # Create vocabulary. Terms will be unigrams (simple words).
  itoken <- text2vec::itoken(tokens, progressbar = FALSE)
  vocab <- text2vec::create_vocabulary(itoken)
  vocab <- text2vec::prune_vocabulary(vocab, term_count_min = 2L)
  
  # Use our filtered vocabulary
  vectorizer <- text2vec::vocab_vectorizer(vocab)
  # use window of 5 for context words
  tcm <- text2vec::create_tcm(itoken, vectorizer, skip_grams_window = 2L)
  
  glove <- text2vec::GlobalVectors$new(word_vectors_size = word2VecSize, 
                                       vocabulary = vocab, x_max = 10, 
                                       learning_rate = 0.1 * 100/word2VecSize,
                                       alpha = 0.75, lambda = 0)
  
  print("creating glove fit")
  glove$fit_transform(tcm, n_iter = 20)
  
  wordVectors <- as.data.table(glove$components)
  
  write.fst(wordVectors, paste0(outPath, "WordVectors-", 
                                subsetSize, "-", word2VecSize,
                                "-", mergeSD, ".fst"))
  
  
  testVector <- wordVectors[, trump]

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
    if (nrow(tmpMatrix) != maxWords) {
      tmpFillUp <- rbind(tmpMatrix,
                         matrix(numeric((maxWords - nrow(tmpMatrix)) * 
                                          channels),
                                ncol = channels))
    } else {
      tmpFillUp <- tmpMatrix
    }
    wordVectorArray[i, , ] <- tmpFillUp
    
  }
  
  print("finished filling")
  
  saveRDS(list(label = label,
               wordVectorArray = wordVectorArray,
               maxWords = maxWords,
               channels = channels),
          file = paste0(outPath, "W2VArray-", 
                        subsetSize, "-", word2VecSize,
                        "-", mergeSD, ".rds"))
  
  
  
  print("sum word vectors to text vectors")
  
  wordVectorSums <- vapply(tokens,FUN.VALUE = numeric(word2VecSize),
                           function(x, data = wordVectors) {
                             cols <- wordVectorsNames[wordVectorsNames %in% x]
                             res <- rowSums(data[, .SD, .SDcols = cols])
                             if(length(res) == 0) return(rep(0, word2VecSize))
                             return(res)
                           })
  
  result = data.table(labelRaw = as.factor(subsetData$category),
                      t(wordVectorSums))
  
  write.fst(result, path = paste0(outPath, "W2V-", 
                                  subsetSize, "-", word2VecSize,
                                  "-", mergeSD, ".fst"))
}

prepareDataEmb = function(inPath = "03_computedData/03_integratedData/", 
                          outPath = "03_computedData/04_preparedData/",
                          subsetSize = c("1pc", "10pc", "100pc"),
                          mergeSD = FALSE){
  assertString(inPath)
  assertString(outPath)
  subsetSize <- match.arg(subsetSize)
  assertFlag(mergeSD)
  
  fileName <- paste0("trainSubset", subsetSize, ".fst")
  subsetData <- read.fst(path = paste0(inPath, fileName), 
                         as.data.table = TRUE)
  
  N <- nrow(subsetData)
  
  label <- subsetData$category
  
  subsetData[, HeadLShortD := paste(headline, short_description, sep = ". ")]
  if (mergeSD) {
    texts <- subsetData$HeadLShortD
  } else {
    texts <- subsetData$headline
  }

  # Create iterator over tokens
  tokens <- quanteda::tokens(texts, what = "word", remove_numbers = FALSE, 
                             remove_punct = TRUE, remove_symbols = TRUE, 
                             remove_hyphens = TRUE)

  # lower all cases
  tokens <- quanteda::tokens_tolower(tokens)
  
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
  maxWords <- max(sortedNumWords[1:2])
  
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

  write.fst(data.table(labelRaw = as.factor(label), padded),
            path = paste0(outPath, "Emb-", 
                                  subsetSize,
                                  "-", mergeSD, ".fst"))
}

