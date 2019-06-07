

processTokens <- function(texts){
  tokens <- tokens(texts, what = "word", remove_numbers = TRUE, 
                   remove_punct = TRUE, remove_symbols = TRUE, 
                   remove_hyphens = TRUE)
  # lower all cases
  tokens <- tokens_tolower(tokens)
  
  # remove stopwords
  tokens <- tokens_remove(tokens, c(stopwords("german"), "dass"))#, "ab"))
  
  # wordstemming
  tokens <- tokens_wordstem(tokens, language = "german")
  
  return(tokens)
}
