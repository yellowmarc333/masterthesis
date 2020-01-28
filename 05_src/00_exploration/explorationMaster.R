# global parameters
fullWidth <- 15.49779
fullHeight <- fullWidth * 9/16
fullWidth2 <- 15.49779 * 3/4
fullHeight2 <- fullWidth * 9/29

# kapitel 2.1 ####
dataRaw <- fread("03_computedData/01_importedData/News.csv")
print(nrow(dataRaw))
print(colnames(dataRaw))
print(length(unique(dataRaw$category)))
print(range(dataRaw$date))

# kapitel 2.2:  klassenmerging ####
##### word intersections (return data in plotWordClouds)
dataRaw <- fread("03_computedData/01_importedData/News.csv")

res <- calcOverlap(dataRaw, nWords = 100)
setorderv(res, "Overlap", -1)
resPrint <- res[c(1:10,13, 14, .N -1, .N)]
write.fst(res, path = "03_computedData/07_deploymentData/categoryOverlap.fst")
print(round(mean(res$Overlap), 2))
print(xtable(resPrint, label = "tab:categoryMerge"), include.rownames = FALSE)

#### should more stopwords be removed?
vocab <- plotWordClouds(dataRaw, "none", nWords = 1000, returnData = TRUE)
# no its okay like that

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
                         dataRaw[category == toupper("parents"), ][1:4, 
                                                                   headline],
                       parenting = 
                         dataRaw[category == toupper("parenting"), ][1:4,
                                                                     headline])
print(xtable(expTable, label = "tab:parentsMerge"), include.rownames = TRUE)


#nicht unterscheidbar
expTable <- data.table(var1 = 
                         dataRaw[category == toupper("green"), ][1:4, headline],
                       var2 = 
                         dataRaw[category == toupper("environment"), ][1:4,
                                                                       headline])
print(xtable(expTable, label = "tab:greenMerge"), include.rownames = TRUE)
#nicht unterscheidbar

expTable <- data.table(var1 = 
                               dataRaw[category == toupper("fifty"), ][1:7, headline],
                       var2 = 
                               dataRaw[category == toupper("parents"), ][1:7,
                                                                             headline])
print(xtable(expTable, label = "tab:greenMerge"), include.rownames = TRUE)
# unterscheidbar

expTable <- data.table(var1 = 
                               dataRaw[category == toupper("taste"), ][1:7, headline],
                       var2 = 
                               dataRaw[category == toupper("food & drink"), ][1:7,
                                                                         headline])
print(xtable(expTable, label = "tab:greenMerge"), include.rownames = TRUE)



# kapitel 2.3 ####
outPath = "03_computedData/07_deploymentData/"
inPath = "03_computedData/02_cleanedData/News.csv"
inPath2 = "03_computedData/02_cleanedData/News.fst"

data <- read.fst(inPath2, as.data.table = TRUE)

ggObj <- barplotCategories(data)
ggsave(filename = paste0(outPath, "barplotCategories.pdf"),
       plot = ggObj, width = fullWidth, height = fullHeight, 
       device = "pdf")

# hier plot und info zu allen Symbols
res <- barplotSymbolInfo(data)
ggObj <- res$ggObj
info <- res$plotData
sapply(info[, .SD, .SDcols = -"category"], function(x) {
        round(mean(x), 3)
})
ggsave(filename = paste0(outPath, "barplotSymbols.pdf"),
       plot = ggObj, width = fullWidth, height = fullHeight, 
       device = "pdf")

# mittlere anzahl wörter pro Kategorie
tableWordInfo <- tableWordInfo(data) # not sure if will be used
print(tableWordInfo[which.max(nWordsByCategory)])
print(tableWordInfo[which.min(nWordsByCategory)])

categoryFreq <- data[, .(count = .N), by = category][order(-count)]
categoryFreq[, cumSumPart := round(cumsum(count)/sum(count), 3)]
print(categoryFreq[6])
print(nrow(data) / length(unique(data$category)))


print(nrow(data))


# evaluation on total data--------------------------------------##
label <- data$category
texts <- as.character(data$headline)

tokens <- quanteda::tokens(texts, what = "word", remove_numbers = FALSE, 
                           remove_punct = FALSE, remove_symbols = FALSE, 
                           remove_hyphens = FALSE)
tokens <- as.list(tokens)

# Create vocabulary. Terms will be unigrams (simple words).
itoken <- text2vec::itoken(tokens, progressbar = FALSE)
vocab <- text2vec::create_vocabulary(itoken)
vocab <- as.data.table(text2vec::prune_vocabulary(vocab, term_count_min = 1L))

# exploration of all tokens
tokensLength <- sapply(tokens, length)
print(round(mean(tokensLength),3)) # wieviele wörter durchschnittlich/schlagzeile
print(min(tokensLength)) # kleinste schlagzeile
print(data[which.min(tokensLength), .(category, headline)]) # wie heißt kleinste schlagzeile

print(max(tokensLength)) # länge maximale schlagzeile
print(head(order(tokensLength, decreasing = TRUE)))
tokens[which.max(tokensLength)] # wie heißt längste schlagzeile


# exploring vocab
print(nrow(vocab)) # anzahl verschiedeener wörter
print(vocab[which.max(term_count)],) # häufigstes wö
print(nrow(vocab[term_count <= 1]))

# how many headlines have 2 sentences
pointOccurance <- vocab[term == ".", doc_count]
print(paste(". occurs", pointOccurance, 
            "times in a sentence"))
print(paste("this equals", pointOccurance/N, "percent of news headlines"))


# wordclouds
wordCloud <- plotWordClouds(data, catFilter = "none", 
                            nWords = 100)
ggsave(filename = paste0(outPath, "wordCloudAll.pdf"),
       plot = wordCloud, width = fullWidth2, height = fullHeight2, 
       device = "pdf")

wordCloud <- plotWordClouds(data, catFilter = "politics", nWords = 100)
ggsave(filename = paste0(outPath, "wordCloudPolitics.pdf"),
       plot = wordCloud, width = fullWidth2, height = fullHeight2, 
       device = "pdf")

wordCloud <- plotWordClouds(data, catFilter = "wellness & healthy living", 
                            nWords = 100)
ggsave(filename = paste0(outPath, "wordCloudWellness.pdf"),
       plot = wordCloud, width = fullWidth, height = fullHeight2, 
       device = "pdf")

wordCloud <- plotWordClouds(data, catFilter = "education", nWords = 100)
ggsave(filename = paste0(outPath, "wordCloudEducation.pdf"),
       plot = wordCloud, width = fullWidth, height = fullHeight, 
       device = "pdf")

# example for bow and tfidf in 

# data <- fread("03_computedData/02_cleanedData/News.csv")
# data <- data[category == "parents"]
# 
# text1 <- data[19, .(headline)][[1]]; text1
# text2 <- data[20, .(headline)][[1]]; text2
# text3 <- data[137, .(headline)][[1]]; text3
# 
# uniqueWords <- unique(strsplit(paste(text1, text2, text3, 
#                                      collapse = " "),
#                                split = " ")[[1]])
# paste(uniqueWords, collapse = " & ")

text1 <- "the cat likes to sit"
text2 <- "the dog does not like his owner"
text3 <- "the owner likes the dog"

texts <- c(text1, text2, text3)

# Create iterator over tokens
tokens <- tokens(texts, what = "word", remove_numbers = FALSE, 
                 remove_punct = FALSE, remove_symbols = FALSE, 
                 remove_hyphens = FALSE)

tokens <- as.list(tokens)

# Create vocabulary. Terms will be unigrams (simple words).
itoken <- text2vec::itoken(tokens, progressbar = FALSE)
vocab <- text2vec::create_vocabulary(itoken)
vocab <- text2vec::prune_vocabulary(vocab, term_count_min = 1L)

# Use our filtered vocabulary
vectorizer <- text2vec::vocab_vectorizer(vocab)
dtm = text2vec::create_dtm(itoken, vectorizer)
dfm = as.dfm(dtm)

tfidf = dfm_tfidf(x = dfm, scheme_tf = "augmented", 
                  scheme_df = "inverse", smoothing = 1,
                  k = 1)

tokens.sparse <- round(tfidf, digits = 3)

