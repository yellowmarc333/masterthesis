
cleanTerms <- function(object){
  # replace some terms that are later not found by global word vectors
  object <- gsub(pattern = "â€Š", replacement = " ",
                          x = object, fixed = TRUE )
  object <- gsub(pattern = "â€™", replacement = "'",
                 x = object, fixed = TRUE )
  object <- gsub(pattern = "n't", replacement = " not",
                          x = object, fixed = TRUE )
  object <- gsub(pattern = "i'm", replacement = "i am",
                          x = object, fixed = TRUE )
  
  # here regex makes sure that no space is before pattern
  object <- gsub(pattern = "(?<! )\\Q'll\\E", replacement = " will",
                 x = object, perl = TRUE)
  object <- gsub(pattern = "(?<! )\\Q've\\E", replacement = " have",
                 x = object, perl = TRUE)
  object <- gsub(pattern = "(?<! )\\Q'd\\E", replacement = " would",
                 x = object, perl = TRUE)
  object <- gsub(pattern = "(?<! )\\Q're\\E", replacement = " are",
                 x = object, perl = TRUE)
  
  object <- gsub(pattern = "â€Š", replacement = " ",
                          x = object, fixed = TRUE )
  object <- gsub(pattern = "it's", replacement = "it is",
                          x = object, fixed = TRUE )
  object <- gsub(pattern = "he's", replacement = "he is",
                 x = object, fixed = TRUE )
  object <- gsub(pattern = "she's", replacement = "she is",
                 x = object, fixed = TRUE )
  object <- gsub(pattern = "here's", replacement = "here is",
                          x = object, fixed = TRUE )
  object <- gsub(pattern = "what's", replacement = "what is",
                          x = object, fixed = TRUE )
  object <- gsub(pattern = "there's", replacement = "there is",
                          x = object, fixed = TRUE )
  object <- gsub(pattern = "let's", replacement = "let us",
                          x = object, fixed = TRUE )
  object <- gsub(pattern = "she's", replacement = "she is",
                          x = object, fixed = TRUE )
  object <- gsub(pattern = "he's", replacement = "he is",
                          x = object, fixed = TRUE )
  object <- gsub(pattern = "that's", replacement = "that is",
                          x = object, fixed = TRUE )
  object <- gsub(pattern = "who's", replacement = "who is",
                 x = object, fixed = TRUE )
  object <- gsub(pattern = "trump's", replacement = "trump his",
                 x = object, fixed = TRUE )


  # i have checked this before, it should work, some symbols
  # still occuring are other symbols
  rmPatterns = c("â","â", "ã", "â", "ê", "#", "˜", "©", "™", "€")
  for (pattern in rmPatterns) {
    object <- gsub(pattern = pattern, replacement = "",
                   x = object, fixed = TRUE)
  }


  # this one at last: patterns like it's or that's should already been removed!
  object <- gsub(pattern = "(?<! )\\Q's\\E", replacement = " its",
                 x = object, perl = TRUE)
  
  object <- gsub(pattern = "-", replacement = " ",
                 x = object, fixed = TRUE )

  res <- paste0(object, collapse = " ")
  
  return(res)
}