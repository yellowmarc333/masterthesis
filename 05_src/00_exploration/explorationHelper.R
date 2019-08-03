plotWordClouds <- function(catFilter = "POLITICS", nWords = 50,
                           returnData = FALSE) {
  
  dataRaw <- read.fst("03_computedData/01_importedData/News.fst",
                   as.data.table = TRUE)
  data <- dataRaw[category == (catFilter),]
  N <- nrow(data)
  
  label <- data$category
  texts <- as.character(data$headline)
  
  # Create iterator over tokens
  tokens <- quanteda::tokens(texts, what = "word", remove_numbers = TRUE, 
                             remove_punct = TRUE, remove_symbols = TRUE, 
                             remove_hyphens = TRUE)
  
  # lower all cases
  tokens <- quanteda::tokens_tolower(tokens)
  # remove stopwords
  tokens <- tokens_remove(tokens, c(stopwords("english")))
  
  tokens <- as.list(tokens)
  
  # Create vocabulary. Terms will be unigrams (simple words).
  itoken <- text2vec::itoken(tokens, progressbar = FALSE)
  vocab <- text2vec::create_vocabulary(itoken)
  vocab <- as.data.table(text2vec::prune_vocabulary(vocab, term_count_min = 2L))
  
  #> Loading required package: ggplot2
  setorderv(vocab, c("term_count", "doc_count"), "-1")
  plotData <- vocab[1:nWords]
  plotData[, plotSize := term_count/sum(term_count)/2]
  plotData[, plotColor := doc_count/sum(doc_count)]
  
  ggObj <- ggplot(plotData, aes(label = term, size = plotSize,
                                color = plotColor)) +
    geom_text_wordcloud() +
    scale_size_area(max_size = 24) +
    theme_minimal()
  
  if (returnData) return(plotData)
  else return(ggObj)
}