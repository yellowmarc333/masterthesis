# next steps:
# für NN normalizierung rein
# binary cross entropy > mse? auschecken?
# find similarities to words to see if word2vec works. (write function)
# check out h2o word2vec
# generell: bei verschiedenen auswahlmöglichkeiten schauen was gut dokumentiert ist
# in word2vec sonderzeichen entfernen bzw stemmen 
# (ueberlegen, was alles raus muss, numbers drin, toLower)
# ueberlegen wie explaining aussehen koennte.
# methoden: multinomial naive bayes
# balancing for training?
# features: number of words, number of words per category, Number of Camel words,
# number of caps
# Number of camel words
# lstm tutorial durchmachen (kerras vignetten)
# recurrent neural net, hierarchical attention neural net
# nach ähnlichen Datensätzen suchen
# markus seine methode im blick behalten
# context2word in python implementieren

# toxicity datensatz ziehen
# framework intelligent neu schreiben prepareData, dann als argument
# welche methode mit parametern.

# xxbenchmarken mit anderen spalten (short description) im datensatz
# xxworld post und the world post zusammenfassen
# xxsubsets einmal wegspeichern und das wars, so samplen dass in der kleinsten
#     klasse erwarte anzahl von beobachtungen xx ist.
# xxbert link öffnen
# xxgrößeren wordvec ausprobieren mit weniger pruning
# xxrandom forest, xxxgboost, 
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


## lstm keras tutorial
detach("package:reticulate", unload = TRUE)
detach("package:kerasR", unload = TRUE)
library(reticulate)
reticulate::use_python("C:\Users\Marc\ANACON~1\envs\R-TENS~1")
reticulate::py_available()
reticulate::py_config()
library(kerasR)
# install_keras()
mod <- kerasR::Sequential()
mod$add(Dense(units = 50, input_shape = 13))

reticulate::import("numpy")



library(keras)
library(tensorflow)
# install_keras()


to_categorical(0:3)

library(reticulate)
scipy <- import("scipy")
scipy$amin(c(1,3,5,7))

Sys.setenv(RETICULATE_PYTHON. = PATH)

site_path = R.home(component = "home")
fname = file.path(site_path, "etc", "Rprofile.site")
file.exists(fname)
file.exists("~/.Rprofile")
list.files(R.home())
list.files(path.expand("~"))
getwd()
touch ~/.Rprofile
Sys.setenv(RETICULATE_PYTHON = "C:/Users/Marc/Anaconda3/python.exe")
library(reticulate)
use_condaenv("myenv")

#### kaggle tutorial ----------------------------------------#######

library(tidyverse) # importing, cleaning, visualising 
library(tidytext) # working with text
library(keras) # deep learning with keras
library(tensorflow)

options(scipen=999) # turn off scientific display

train <- fread("/Users/Marc/Downloads/quoraMachineLearning/train.csv", data.table = FALSE)
test <- fread("/Users/Marc/Downloads/quoraMachineLearning/test.csv", data.table = FALSE)
train <- train[1:500,]
test <- test[1:300,]

max_words <- 15000 # Maximum number of words to consider as features
maxlen <- 64 # Text cutoff after n words


# Prepare to tokenize the text

full <- rbind(train %>% select(question_text), test %>% select(question_text))
texts <- full$question_text

tokenizer <- text_tokenizer(num_words = max_words) %>% 
  fit_text_tokenizer(texts)

# Tokenize - i.e. convert text into a sequence of integers

sequences <- texts_to_sequences(tokenizer, texts)
word_index <- tokenizer$word_index

# Pad out texts so everything is the same length

data = pad_sequences(sequences, maxlen = maxlen)

train_matrix = data[1:nrow(train),]
test_matrix = data[(nrow(train)+1):nrow(data),]


# Prepare training labels

labels = train$target


# Prepare a validation set

set.seed(1337)

training_samples = nrow(train_matrix)*0.90
validation_samples = nrow(train_matrix)*0.10

indices = sample(1:nrow(train_matrix))
training_indices = indices[1:training_samples]
validation_indices = indices[(training_samples + 1): (training_samples + validation_samples)]

x_train = train_matrix[training_indices,]
y_train = labels[training_indices]

x_val = train_matrix[validation_indices,]
y_val = labels[validation_indices]

# Training dimensions

dim(x_train)
View(head(x_train))
table(y_train)

lines <- readLines("/Users/Marc/Downloads/quoraMachineLearning/wiki-news-300d-1M/wiki-news-300d-1M.vec")

fastwiki_embeddings_index = new.env(hash = TRUE, parent = emptyenv())

lines <- lines[2:length(lines)]

pb <- txtProgressBar(min = 0, max = length(lines), style = 3)
testfct <- function() {
  
  browser()
  for (i in 1:length(lines)){
    line <- lines[[i]]
    values <- strsplit(line, " ")[[1]]
    word<- values[[1]]
    fastwiki_embeddings_index[[word]] = as.double(values[-1])
    setTxtProgressBar(pb, i)
  }
  
  # Create our embedding matrix
  fastwiki_embedding_dim = 300
  fastwiki_embedding_matrix = array(0, c(max_words, fastwiki_embedding_dim))
  
  for (word in names(word_index)){
    index <- word_index[[word]]
    if (index < max_words){
      fastwiki_embedding_vector = fastwiki_embeddings_index[[word]]
      if (!is.null(fastwiki_embedding_vector))
        fastwiki_embedding_matrix[index+1,] <- fastwiki_embedding_vector # Words without an embedding are all zeros
    }
  }

}

testfct()
gc()

input <- layer_input(
  shape = list(NULL),
  dtype = "int32",
  name = "input"
)


embedding <- input %>% 
  layer_embedding(input_dim = max_words, output_dim = fastwiki_embedding_dim,
                  name = "embedding")

lstm <- embedding %>% 
  layer_lstm(units = maxlen,dropout = 0.25, recurrent_dropout = 0.25, return_sequences = FALSE, name = "lstm")

dense <- lstm %>%
  layer_dense(units = 128, activation = "relu", name = "dense") 

predictions <- dense %>% 
  layer_dense(units = 1, activation = "sigmoid", name = "predictions")


# Bring model together

model <- keras_model(input, predictions)

# Freeze the embedding weights initially to prevent updates propgating back through and ruining our embedding

get_layer(model, name = "embedding") %>% 
  set_weights(list(fastwiki_embedding_matrix)) %>% 
  freeze_weights()


# Compile

model %>% compile(
  optimizer = optimizer_adam(),
  loss = "binary_crossentropy",
  metrics = "binary_accuracy"
)


# Print architecture (plot_model isn't implemented in the R package yet)

print(model)



history <- model %>% fit(
  as.matrix(x_train),
  as.matrix(y_train),
  batch_size = 2048,
  validation_data = list(as.matrix(x_val), as.matrix(y_val)),
  epochs = 35,
  view_metrics = FALSE,
  verbose = 0
)

# Look at training results

print(history)
plot(history)

### imdb cnn example

max_features <- 5000
maxlen <- 400
batch_size <- 32
embedding_dims <- 50
filters <- 250
kernel_size <- 3
hidden_dims <- 250
epochs <- 2

imdb <- dataset_imdb(num_words = max_features)


