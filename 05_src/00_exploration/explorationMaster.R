# global parameters
fullWidth <- 15.49779
fullHeight <- fullWidth * 9/16

# kapitel 2.1 ####
dataRaw <- fread("03_computedData/01_importedData/News.csv")
print(nrow(dataRaw))
print(colnames(dataRaw))
print(length(unique(dataRaw$category)))
print(range(dataRaw$date))

# kapitel 2.2:  klassenmerging ####
##### word intersections (return data in plotWordClouds)
word1 <- plotWordClouds(dataRaw, catFilter = "PARENTS", nWords = 50, 
                        returnData = TRUE)
word2 <- plotWordClouds(dataRaw, catFilter = "PARENTING", nWords = 50,
                        returnData = TRUE)
length(intersect(word1$term, word2$term))/50 # 0.74

word1 <- plotWordClouds(dataRaw, catFilter = "WORLDPOST", nWords = 50, 
                        returnData = TRUE)
word2 <- plotWordClouds(dataRaw, catFilter = "THE WORLDPOST", nWords = 50,
                        returnData = TRUE)
length(intersect(word1$term, word2$term))/50  # 0.46

word1 <- plotWordClouds(dataRaw, catFilter = "CULTURE & ARTS", nWords = 50, 
                        returnData = TRUE)
word2 <- plotWordClouds(dataRaw, catFilter = "ARTS & CULTURE", nWords = 50,
                        returnData = TRUE) #0.38
length(intersect(word1$term, word2$term))/50 

word1 <- plotWordClouds(dataRaw, catFilter = "ARTS", nWords = 50, 
                        returnData = TRUE)
word2 <- plotWordClouds(dataRaw, catFilter = "ARTS & CULTURE", nWords = 50,
                        returnData = TRUE) #0.4
length(intersect(word1$term, word2$term))/50 

word1 <- plotWordClouds(dataRaw, catFilter = "ARTS", nWords = 50, 
                        returnData = TRUE)
word2 <- plotWordClouds(dataRaw, catFilter = "CULTURE & ARTS", nWords = 50,
                        returnData = TRUE) # 0.52
length(intersect(word1$term, word2$term))/50 

word1 <- plotWordClouds(dataRaw, catFilter = "STYLE", nWords = 50, 
                        returnData = TRUE)
word2 <- plotWordClouds(dataRaw, catFilter = "STYLE & BEAUTY", nWords = 50,
                        returnData = TRUE)
length(intersect(word1$term, word2$term))/50 #  0.52

word1 <- plotWordClouds(dataRaw, catFilter = "GREEN", nWords = 50, 
                        returnData = TRUE)
word2 <- plotWordClouds(dataRaw, catFilter = "ENVIRONMENT", nWords = 50,
                        returnData = TRUE)
length(intersect(word1$term, word2$term))/50 # 0.5

# beispiel für zwei unterscheidbare kategorien
word1 <- plotWordClouds(dataRaw, catFilter = "WELLNESS", nWords = 50, 
                        returnData = TRUE)
word2 <- plotWordClouds(dataRaw, catFilter = "BLACK VOICES", nWords = 50,
                        returnData = TRUE)
length(intersect(word1$term, word2$term))/50 # 0.16

word1 <- plotWordClouds(dataRaw, catFilter = "POLITICS", nWords = 50, 
                        returnData = TRUE)
word2 <- plotWordClouds(dataRaw, catFilter = "HOME & LIVING", nWords = 50,
                        returnData = TRUE)
length(intersect(word1$term, word2$term))/50 # 0.06



# beispiele mit menschlicher intuition
dataRaw <- fread("03_computedData/01_importedData/News.csv")
expTable <- data.table(var1 = 
                         dataRaw[category == toupper("worldpost"), ][1:4, headline],
                       var2 = 
                         dataRaw[category == toupper("the worldpost"), ][1:4, headline])
print(xtable(expTable, label = "tab:worldMerge"), include.rownames = TRUE)

# nicht unterscheidbar
expTable <- data.table(var1 = 
                         dataRaw[category == toupper("culture & arts"), ][1:4, headline],
                       var2 = 
                         dataRaw[category == toupper("arts & culture"), ][1:4, headline],
                       var3 = 
                         dataRaw[category == toupper("arts"), ][1:4, headline])
print(xtable(expTable, label = "tab:artsMerge"), include.rownames = TRUE)

# nicht unterscheidbar
expTable <- data.table(var1 = 
                         dataRaw[category == toupper("style"), ][1:4, headline],
                       var2 = 
                         dataRaw[category == toupper("style & beauty"), ][1:4, headline])
print(xtable(expTable, label = "tab:styleMerge"), include.rownames = TRUE)
# nicht unterscheidbar

expTable <- data.table(parents = 
                         dataRaw[category == toupper("parents"), ][1:4, headline],
                       parenting = 
                         dataRaw[category == toupper("parenting"), ][1:4, headline])
print(xtable(expTable, label = "tab:parentsMerge"), include.rownames = TRUE)


#nicht unterscheidbar
expTable <- data.table(var1 = 
                         dataRaw[category == toupper("green"), ][1:4, headline],
                       var2 = 
                         dataRaw[category == toupper("environment"), ][1:4, headline])
print(xtable(expTable, label = "tab:greenMerge"), include.rownames = TRUE)
#nicht unterscheidbar

# kapitel 2.3 ####
inPath = "03_computedData/02_cleanedData/News.fst"
outPath = "03_computedData/07_deploymentData/"
data <- read.fst(inPath, as.data.table = TRUE)

ggObj <- barplotCategories(data)
ggsave(filename = paste0(outPath, "barplotCategories.pdf"),
       plot = ggObj, width = fullWidth, height = fullHeight, 
       device = "pdf")

categoryFreq <- data[, .(count = .N), by = category][order(-count)]
categoryFreq[, cumSumPart := round(cumsum(count)/sum(count), 3)]
print(categoryFreq[6])
print(nrow(data) / length(unique(data$category)))

# average number of words by category: (do in)
nWordsByCategory <- data[, {
  tokens <- quanteda::tokens(headline, what = "word", remove_numbers = FALSE, 
                             remove_punct = FALSE, remove_symbols = FALSE, 
                             remove_hyphens = TRUE)
  tokens <- as.list(tokens)
  res <- round(mean(sapply(tokens, length)), digits = 3)
  .(nWordsByCategory = res)
}, by = category]

setorderv(nWordsByCategory, c("nWordsByCategory"), -1)
# anzahl symbole, anzahl neuangefangene Sätze anzahl stopwords

print(nrow(data))
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


# wordclouds
wordCloud <- plotWordClouds(data, catFilter = "politics", nWords = 100)
ggsave(filename = paste0(outPath, "wordCloudPolitics.pdf"),
       plot = wordCloud, width = fullWidth, height = fullHeight, 
       device = "pdf")

wordCloud <- plotWordClouds(data, catFilter = "wellness", nWords = 100)
ggsave(filename = paste0(outPath, "wordCloudWellness.pdf"),
       plot = wordCloud, width = fullWidth, height = fullHeight, 
       device = "pdf")

wordCloud <- plotWordClouds(data, catFilter = "education", nWords = 100)
ggsave(filename = paste0(outPath, "wordCloudEducation.pdf"),
       plot = wordCloud, width = fullWidth, height = fullHeight, 
       device = "pdf")





