data <- read.fst("03_computedData/01_importedData/News.fst",
                 as.data.table = TRUE)
N <- nrow(data)

label <- data$category

texts <- as.character(data$headline)

# Create iterator over tokens
tokens <- quanteda::tokens(texts, what = "word", remove_numbers = FALSE, 
                           remove_punct = FALSE, remove_symbols = FALSE, 
                           remove_hyphens = TRUE)

# lower all cases
tokens <- quanteda::tokens_tolower(tokens)
tokens <- as.list(tokens)

# Create vocabulary. Terms will be unigrams (simple words).
itoken <- text2vec::itoken(tokens, progressbar = FALSE)
vocab <- text2vec::create_vocabulary(itoken)
vocab <- as.data.table(text2vec::prune_vocabulary(vocab, term_count_min = 2L))

# how many headlines have 2 sentences
pointOccurance <- vocab[term == ".", doc_count]
print(paste(". occurs", pointOccurance, 
            "times in a senctence"))
print(paste("this equals", pointOccurance/N, "percent of news headlines"))

# plot for category frequencies
categoryFreq <- data[, .(count = .N), by = category]
categoryFreq[, labPos := cumsum(count) - 0.5*count]


ggObj <- ggplot(categoryFreq, aes(x = 2, y = count, fill = category)) +
  geom_bar(stat = "identity", color = "black") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = labPos, label = count), color = "white")+
  #scale_fill_manual(palette = "blues") +
  theme_void() +
  xlim(0.5, 2.5) 
ggObj



wordCloud1 <- plotWordClouds(catFilter = "PARENTS", nWords = 50)
wordCloud2 <- plotWordClouds(catFilter = "PARENTING", nWords = 50)
wordCloudComb <- ggpubr::ggarrange(wordCloud1, wordCloud2,
                                   nrow = 1)

wordCloud1 <- plotWordClouds(catFilter = "CULTURE & ARTS", nWords = 50)
wordCloud2 <- plotWordClouds(catFilter = "ARTS & CULTURE", nWords = 50)
wordCloudComb <- ggpubr::ggarrange(wordCloud1, wordCloud2,
                                   nrow = 1)

wordCloud1 <- plotWordClouds(catFilter = "GREEN", nWords = 50)
wordCloud2 <- plotWordClouds(catFilter = "ENVIRONMENT", nWords = 50)
wordCloudComb <- ggpubr::ggarrange(wordCloud1, wordCloud2,
                                   nrow = 1)

wordCloud1 <- plotWordClouds(catFilter = "STYLE", nWords = 50)
wordCloud2 <- plotWordClouds(catFilter = "STYLE & BEAUTY", nWords = 50)
wordCloudComb <- ggpubr::ggarrange(wordCloud1, wordCloud2,
                                   nrow = 1)

##### data
word1 <- plotWordClouds(catFilter = "PARENTS", nWords = 50, 
                             returnData = TRUE)
word2 <- plotWordClouds(catFilter = "PARENTING", nWords = 50,
                             returnData = TRUE)
length(intersect(word1$term, word2$term))/50 # 0.74

word1 <- plotWordClouds(catFilter = "WORLDPOST", nWords = 50, 
                        returnData = TRUE)
word2 <- plotWordClouds(catFilter = "THE WORLDPOST", nWords = 50,
                        returnData = TRUE)
length(intersect(word1$term, word2$term))/50 

word1 <- plotWordClouds(catFilter = "CULTURE & ARTS", nWords = 50, 
                        returnData = TRUE)
word2 <- plotWordClouds(catFilter = "ARTS & CULTURE", nWords = 50,
                        returnData = TRUE)
length(intersect(word1$term, word2$term))/50 

word1 <- plotWordClouds(catFilter = "STYLE", nWords = 50, 
                        returnData = TRUE)
word2 <- plotWordClouds(catFilter = "STYLE & BEAUTY", nWords = 50,
                        returnData = TRUE)
length(intersect(word1$term, word2$term))/50 #  0.52


dataRaw <- read.fst("03_computedData/01_importedData/News.fst",
                    as.data.table = TRUE)
dataRaw[category == "WORLDPOST", ][1:5, headline]
dataRaw[category == "THE WORLDPOST", ][1:5, headline]
# nicht unterscheidbar

dataRaw[category == "CULTURE & ARTS", ][1:5, headline]
dataRaw[category == "ARTS & CULTURE", ][1:5, headline]
# nicht unterscheidbar

dataRaw[category == "STYLE", ][1:5, headline]
dataRaw[category == "STYLE & BEAUTY", ][1:5, headline]
# nicht unterscheidbar

dataRaw[category == "PARENTS", ][1:5, headline]
dataRaw[category == "PARENTING", ][1:5, headline]
#nicht unterscheidbar