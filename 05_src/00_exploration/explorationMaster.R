# global parameters
fullWidth <- 15.49779
fullHeight <- fullWidth * 9/16

# kapitel 2.1 ####
data <- fread("03_computedData/01_importedData/News.csv")
print(nrow(data))
print(colnames(data))
print(length(unique(data$category)))

# kapitel 2.3 ####
inPath = "03_computedData/02_cleanedData/News.fst"
outPath = "03_computedData/07_deploymentData/"
data <- read.fst(inPath, as.data.table = TRUE)

ggObj <- barplotCategories(data)
ggsave(filename = paste0(outPath, "barplotCategories.pdf"),
       plot = ggObj, width = fullWidth, height = fullHeight, 
       device = "pdf")


N <- nrow(data)
label <- data$category
texts <- as.character(data$headline)

# Create iterator over tokens
tokens <- quanteda::tokens(texts, what = "word", remove_numbers = FALSE, 
                           remove_punct = FALSE, remove_symbols = FALSE, 
                           remove_hyphens = TRUE)

tokens <- as.list(tokens)

# Create vocabulary. Terms will be unigrams (simple words).
itoken <- text2vec::itoken(tokens, progressbar = FALSE)
vocab <- text2vec::create_vocabulary(itoken)
vocab <- as.data.table(text2vec::prune_vocabulary(vocab, term_count_min = 2L))


# how many headlines have 2 sentences
pointOccurance <- vocab[term == ".", doc_count]
print(paste(". occurs", pointOccurance, 
            "times in a sentence"))
print(paste("this equals", pointOccurance/N, "percent of news headlines"))



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
length(intersect(word1$term, word2$term))/50  # 0.46

word1 <- plotWordClouds(catFilter = "CULTURE & ARTS", nWords = 50, 
                        returnData = TRUE)
word2 <- plotWordClouds(catFilter = "ARTS & CULTURE", nWords = 50,
                        returnData = TRUE) #0.38
length(intersect(word1$term, word2$term))/50 

word1 <- plotWordClouds(catFilter = "STYLE", nWords = 50, 
                        returnData = TRUE)
word2 <- plotWordClouds(catFilter = "STYLE & BEAUTY", nWords = 50,
                        returnData = TRUE)
length(intersect(word1$term, word2$term))/50 #  0.52

word1 <- plotWordClouds(catFilter = "GREEN", nWords = 50, 
                        returnData = TRUE)
word2 <- plotWordClouds(catFilter = "ENVIRONMENT", nWords = 50,
                        returnData = TRUE)
length(intersect(word1$term, word2$term))/50 # 0.5


# kapitel 2.2:  klassenmerging
dataRaw <- read.fst("03_computedData/02_cleanedData/News.fst",
                    as.data.table = TRUE)
dataRaw[category == "worldpost", ][1:4, headline]
dataRaw[category == "the worldpost", ][1:4, headline]
# nicht unterscheidbar

dataRaw[category == "culture & arts", ][1:4, headline]
dataRaw[category == "arts & culture", ][1:4, headline]
dataRaw[category == "arts", ][1:4, headline]
# nicht unterscheidbar

dataRaw[category == "style", ][1:4, headline]
dataRaw[category == "style & beauty", ][1:4, headline]
# nicht unterscheidbar

expTable <- data.table(parents = 
                     dataRaw[category == "parents", ][1:4, headline],
                     parenting = 
                       dataRaw[category == "parenting", ][1:4, headline])
print(xtable(expTable, label = "tab:parentsMerge"), include.rownames = TRUE)


#nicht unterscheidbar

dataRaw[category == "green", ][1:4, headline]
dataRaw[category == "environment", ][1:4, headline]
#nicht unterscheidbar