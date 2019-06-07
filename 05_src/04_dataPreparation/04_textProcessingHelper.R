processTokens <- function(texts){
  tokens <- quanteda::tokens(texts, what = "word", remove_numbers = F, 
                   remove_punct = TRUE, remove_symbols = TRUE, 
                   remove_hyphens = TRUE)
  # lower all cases
  tokens <- quanteda::tokens_tolower(tokens)
  
  # remove stopwords
  tokens <- quanteda::tokens_remove(tokens, c(stopwords("en"))) 
  
  # wordstemming
  tokens <- quanteda::tokens_wordstem(tokens, language = "en")
  
  return(tokens)
}
