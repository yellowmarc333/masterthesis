prepareDataBOW = function(inPath = "03_computedData/02_cleanedData/", 
                       outPath = "03_computedData/04_preparedData/",
                       subsetSize = 0.01,
                       mergeSD = FALSE){
  assertString(inPath)
  assertString(outPath)
  assertNumber(subsetSize, lower = 0, upper = 1)
  assertFlag(mergeSD)
  
  data = read.fst(path = paste0(inPath, "News.fst"), as.data.table = T)
  set.seed(123)
  subsetData <- data[sample.int(.N, floor(.N * subsetSize))]
  if (subsetSize == 1) subsetData = data
  
  label <- oneHotEncode(subsetData$category)
  labelRaw <- as.factor(subsetData$category)
  subsetData[, HeadLShortD := paste(headline, short_description, sep = ". ")]
  if (mergeSD) {
    texts <- subsetData$HeadLShortD
  } else {
    texts <- subsetData$headline
  }
  tokens <- processTokens(texts)
  
  # reduce to tokens > 0
  hasWords <- lengths(tokens) > 0
  tokens <- tokens[hasWords]
  label = label[hasWords]
  labelRaw = labelRaw[hasWords]
  
  tokens.dfm <- dfm(tokens)
  
  tokens.dfm <- quanteda::dfm_trim(tokens.dfm, min_docfreq = 5)
  tokens.dt <- as.data.table(quanteda::convert(tokens.dfm, to = "data.frame"))
  tokens.dt[, document := NULL]
  tokens.dt.label <- data.table(labelRaw, tokens.dt)
  
  write.fst(tokens.dt.label, path = paste0(outPath, "BOW-", 
                                           subsetSize, "-", mergeSD, ".fst"))
}



prepareDataW2V = function(inPath = "03_computedData/02_cleanedData/", 
                          outPath = "03_computedData/04_preparedData/",
                          subsetSize = 0.01,
                          word2VecSize = 100,
                          mergeSD = FALSE){
  assertString(inPath)
  assertString(outPath)
  assertNumber(word2VecSize)
  assertNumber(subsetSize, lower = 0, upper = 1)
  assertFlag(mergeSD)
  
  
  data <- read.fst(path = paste0(inPath, "News.fst"), as.data.table = TRUE)
  set.seed(123)
  subsetData = data[sample.int(.N, floor(.N * subsetSize))]
  if(subsetSize == 1) subsetData = data
  
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
  tokens <- as.list(tokens)
  maxWords <- max(sapply(tokens, length))
  # old method
  # tokens <- text2vec::space_tokenizer(text)

  # Create vocabulary. Terms will be unigrams (simple words).
  itoken <- text2vec::itoken(tokens, progressbar = FALSE)
  vocab <- create_vocabulary(itoken)
  vocab <- prune_vocabulary(vocab, term_count_min = 5L)
  
  # Use our filtered vocabulary
  vectorizer <- text2vec::vocab_vectorizer(vocab)
  # use window of 5 for context words
  tcm <- create_tcm(itoken, vectorizer, skip_grams_window = 4L)

  glove <- GlobalVectors$new(word_vectors_size = word2VecSize, 
                            vocabulary = vocab, x_max = 10, 
                            learning_rate = 0.15 * 100/word2VecSize,
                            alpha = 0.75, lambda = 0)
  
  print("creating glove fit")
  glove$fit_transform(tcm, n_iter = 25)
  
  wordVectors <- as.data.table(glove$components)
  
  write.fst(wordVectors, paste0(outPath, "WordVectors-", 
                                subsetSize, "-", word2VecSize,
                                "-", mergeSD, ".fst"))
  # testVector <- wordVectors[,Ginger]
  # 
  # cos_sim = sapply(data.table(wordVectors), function(x) {
  #   sum((x - testVector)^2)
  # })
  # cos_sim = sim2(x = t(wordVectors), y = t(testVector), method = "cosine",
  #                norm = "l2")
  # head(sort(cos_sim[,1], decreasing = TRUE), 20)

  # hier werden schon die seltenen woerter nicht genutzt
  wordVectorsNames <- colnames(wordVectors)
  channels <- nrow(wordVectors)
  
  print("creating wordVectorArray")
  wordVectorArray <- array(numeric(nrow(subsetData)* maxWords * channels),
                           dim = c(nrow(subsetData), maxWords, channels))
  print("fill wordVectorArray")

  for(i in seq_len(nrow(subsetData))) {
    
    tmpWords <- tokens[[i]]
    existWords <- tmpWords[tmpWords %in% wordVectorsNames]
    if(length(existWords) == 0){
      tmpMatrix <- matrix(numeric(channels),
                                 ncol = channels)
      
    } else{
      tmpMatrix <- as.matrix(t(wordVectors[, .SD, .SDcols = existWords]))
    }
    # if there are no existWords, still initialize a matrix
    
    # fill up rows with 0's for equal array length, when length < maxWords
    if (nrow(tmpMatrix) != maxWords) {
      tmpFillUp <- rbind(tmpMatrix,
                         matrix(numeric((maxWords - length(existWords)) * 
                                          channels),
                                ncol = channels))
    } else {
      if(!identical(c(maxWords, channels), dim(tmpMatrix))) browser()
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


