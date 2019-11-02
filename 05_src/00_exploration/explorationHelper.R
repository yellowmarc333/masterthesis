plotWordClouds <- function(data, catFilter = "POLITICS", nWords = 50,
                           returnData = FALSE) {
  assertDataTable(data)
  data <- data[category == (catFilter),]
  N <- nrow(data)
  
  label <- data$category
  texts <- as.character(data$headline)
  
  # Create iterator over tokens
  tokens <- quanteda::tokens(texts, what = "word", remove_numbers = TRUE, 
                             remove_punct = TRUE, remove_symbols = TRUE, 
                             remove_hyphens = TRUE)
  
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
  plotData[, plotColor := sample(brewer.pal(8, "Dark2"), .N,
                                 replace = TRUE)]
  # plotData[, term := c("Marc", "schmieder", "yellow", "smart", "333",
  #                      "smartmarc333", "krass", "klug", "skilled", "gentle",
  #                      "music", "poker", "statistik", "math", "genius",
  #                      "empathic", "industrie 4.0", "jokes", "chips", "guitar",
  #                      "keys", "visionary", "music genius", "solving", "problems")]
  # alternative use
  #brewer.pal(8, "Dark2")
  #topo.colors(10)
  # wordcloud(words = plotData$term, freq = plotData$term_count,
  #           min.freq = 1, 
  #           max.words = 50,
  #           random.order = FALSE, rot.per = 0.35,
  #           colors = brewer.pal(8, "Dark2"))
  # plotData2 <- copy(plotData)
  # setnames(plotData2, c("term", "term_count"), c("word", "freq"))
  # 
  # wordcloud2(data = plotData, size = 1.6, 
  #                   color = 'random-dark')
  # 
  #barplot(1:100, col = plotData$plotColor)
  ggObj <- ggplot(plotData, aes(label = term, size = plotSize)) +
    geom_text_wordcloud(shape = "square", color = plotData$plotColor) +
    scale_size_area(max_size = 24) +
    theme_minimal()
  
  if (returnData) return(plotData)
  else return(ggObj)
}



colGenerator <- colorRampPalette(c("grey", "yellow", "red",
                                   "mediumorchid1", "green", 
                                   "royalblue1", "black"))

barplotCategories <- function(data){
  assertDataTable(data)

   # plot for category frequencies
  categoryFreq <- data[, .(count = .N), by = category]
  categoryFreq[, labPos := cumsum(count) - 0.5*count]
  
  ggObj <- ggplot(categoryFreq, aes(x = reorder(category, -count),
                                    y = count, fill = category, 
                                    label = category)) +
    geom_bar(stat = "identity", fill =  rev(colGenerator(nrow(categoryFreq)))) +
    geom_text_repel(nudge_x = 2, nudge_y = 10000, segment.size = 0.8,
                    segment.alpha = 0.5, point.padding = 2.5,
                    box.padding = 0.5, size = 7) +
    geom_text(aes(x = category, y = count, label = count),
              vjust = -0.5, angle = 0, size = 4) +
    labs(x = "Nachrichtenkategorie",
         y = "Anzahl Datenpunkte") +
    theme(axis.text.x  = element_blank(),
          axis.title = element_text(size = 20),
          axis.ticks.x = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())  
  
  return(ggObj)
}


lineplotCategories <- function(data){
  assertDataTable(data)
  
  # plot for category frequencies
  categoryFreq <- data[, .(count = .N), by = category]
  categoryFreq[, labPos := cumsum(count) - 0.5*count]
  
  ggObj <- ggplot(categoryFreq, aes(x = reorder(category, -count),
                                    y = count, fill = category, 
                                    label = category)) +
    geom_bar(stat = "identity", fill =  rev(colGenerator(nrow(categoryFreq)))) +
    geom_text_repel(nudge_x = 2, nudge_y = 10000, segment.size = 0.8,
                    segment.alpha = 0.5, point.padding = 2.5,
                    box.padding = 0.5, size = 7) +
    geom_text(aes(x = category, y = count, label = count),
              vjust = -0.5, angle = 0, size = 4) +
    labs(x = "Nachrichtenkategorie",
         y = "Anzahl Datenpunkte") +
    theme(axis.text.x  = element_blank(),
          axis.title = element_text(size = 20),
          axis.ticks.x = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())  
  
  return(ggObj)
}
