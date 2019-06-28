# next steps:
# für NN normalizierung rein
# ueberlegen wie explaining aussehen koennte.
# liste machen mit methoden die ausprobiert werden können
# random forest, xxxgboost, multinomial naive bayes
# recurrent neural net, hierarchical attention neural net
# nach ähnlichen Datensätzen suchen
# markus seine methode im blick behalten
# benchmarken mit anderen spalten im datensatz
# toxicity datensatz ziehen
# framework intelligent neu schreiben prepareData, dann als argument
# welche methode mit parametern.
# world post und the world post zusammenfassen
# xxbert link öffnen

# xxprepareWord2Vec 
# xxsetup random forest
# xxset up h2o to work
# xgboost: add watchlist
# xxset up error rate for h20
# xxrename column names from data in preparation (nicht notwendig)
# xxadd error measurement of the network
# xx3blue one brown deep learning serie anschauen + notizen



download.file("https://snap.stanford.edu/data/finefoods.txt.gz", "finefoods.txt.gz")
library(readr)
library(stringr)
reviews <- read_lines("finefoods.txt.gz") 
reviews <- reviews[str_sub(reviews, 1, 12) == "review/text:"]
reviews <- str_sub(reviews, start = 14)
reviews <- iconv(reviews, to = "UTF-8")

head(reviews)

library(keras)
library(tensorflow)
tokenizer <- text_tokenizer(num_words = 20000)
tokenizer %>% fit_text_tokenizer(reviews)


###### Glove example
library(text2vec)
text8_file = "~/text8"
if (!file.exists(text8_file)) {
  download.file("http://mattmahoney.net/dc/text8.zip", "~/text8.zip")
  unzip ("~/text8.zip", files = "text8", exdir = "~/")
}
wiki = readLines(text8_file, n = 1, warn = FALSE)

# Create iterator over tokens
tokens <- text2vec::space_tokenizer(wiki)
# Create vocabulary. Terms will be unigrams (simple words).
it = text2vec::itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)
vocab <- prune_vocabulary(vocab, term_count_min = 5L)

# Use our filtered vocabulary
vectorizer <- text2vec::vocab_vectorizer(vocab)
# use window of 5 for context words
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5L)

glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)
glove$fit_transform(tcm, n_iter = 20)

word_vectors <- glove$components

berlin <- word_vectors[,"paris", drop = FALSE] - 
  word_vectors[, "france", drop = FALSE] + 
  word_vectors[, "germany" , drop = FALSE]
cos_sim = sapply(data.table(word_vectors), function(x) {
  sum((x - berlin)^2)
})
cos_sim = sim2(x = t(word_vectors), y = t(berlin), method = "cosine", 
               norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 5)

