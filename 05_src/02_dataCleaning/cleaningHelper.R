
cleanTerms <- function(object){
  # replace some terms that are later not found by global word vectors
  object <- gsub(pattern = "â€Š", replacement = " ",
                          x = object, fixed = TRUE )
  object <- gsub(pattern = "n't", replacement = " not",
                          x = object, fixed = TRUE )
  object <- gsub(pattern = "'re", replacement = " are",
                          x = object, fixed = TRUE )
  object <- gsub(pattern = "i'm", replacement = "i am",
                          x = object, fixed = TRUE )
  object <- gsub(pattern = "'ll", replacement = " will",
                          x = object, fixed = TRUE )
  object <- gsub(pattern = "'ve", replacement = " have",
                          x = object, fixed = TRUE )
  object <- gsub(pattern = "'d", replacement = " would",
                          x = object, fixed = TRUE )
  object <- gsub(pattern = "â€Š", replacement = " ",
                          x = object, fixed = TRUE )
  object <- gsub(pattern = "it's", replacement = "it is",
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
  # from here all the o to c error
  # object <- gsub(pattern = "clympic", replacement = "olympic",
  #                x = object, fixed = TRUE )
  # object <- gsub(pattern = "phctcs", replacement = "photos",
  #                x = object, fixed = TRUE )
  # object <- gsub(pattern = "videc", replacement = "video",
  #                x = object, fixed = TRUE )
  # object <- gsub(pattern = "cver", replacement = "over",
  #                x = object, fixed = TRUE )
  # object <- gsub(pattern = "cbama", replacement = "obama",
  #                x = object, fixed = TRUE )
  # object <- gsub(pattern = "phctc", replacement = "photo",
  #                x = object, fixed = TRUE )
  # object <- gsub(pattern = "cther", replacement = "other",
  #                x = object, fixed = TRUE )
  # object <- gsub(pattern = "cnly", replacement = "only",
  #                x = object, fixed = TRUE )
  # object <- gsub(pattern = "cpen", replacement = "open",
  #                x = object, fixed = TRUE )
  # object <- gsub(pattern = "cwn", replacement = "own",
  #                x = object, fixed = TRUE )
  # object <- gsub(pattern = "cffice", replacement = "office",
  #                x = object, fixed = TRUE )
  # object <- gsub(pattern = "cbamacare", replacement = "obamacare",
  #                x = object, fixed = TRUE )
  # object <- gsub(pattern = "cnline", replacement = "online",
  #                x = object, fixed = TRUE )
  # object <- gsub(pattern = "cscar", replacement = "oscar",
  #                x = object, fixed = TRUE )
  # object <- gsub(pattern = "crder", replacement = "order",
  #                x = object, fixed = TRUE )
  # object <- gsub(pattern = "cnce", replacement = "cnce",
  #                x = object, fixed = TRUE )
  # object <- gsub(pattern = "clympics", replacement = "olympics",
  #                x = object, fixed = TRUE )
  # object <- gsub(pattern = "cfficial", replacement = "official",
  #                x = object, fixed = TRUE )
  # object <- gsub(pattern = "cscars", replacement = "oscars",
  #                x = object, fixed = TRUE )
  # object <- gsub(pattern = "cffers", replacement = "offers",
  #                x = object, fixed = TRUE )
  # object <- gsub(pattern = "cfficials", replacement = "officials",
  #                x = object, fixed = TRUE )
  # object <- gsub(pattern = "cutside", replacement = "outside",
  #                x = object, fixed = TRUE )
  # object <- gsub(pattern = "clympics", replacement = "olympics",
  #                x = object, fixed = TRUE )
  # object <- gsub(pattern = "divcrce", replacement = "divorce",
  #                x = object, fixed = TRUE )
  # object <- gsub(pattern = "reccrds", replacement = "records",
  #                x = object, fixed = TRUE )
  # object <- gsub(pattern = "divcrce", replacement = "divorce",
  #                x = object, fixed = TRUE )
  # object <- gsub(pattern = "reccrd", replacement = "record",
  #                x = object, fixed = TRUE )
  # object <- gsub(pattern = "ccean", replacement = "ocean",
  #                x = object, fixed = TRUE )
  # object <- gsub(pattern = "cctober", replacement = "october",
  #                x = object, fixed = TRUE )

  
  rmPatterns = c("â", "ã", "â", "ê", "#", "˜", "©", "™", "€")
  for (pattern in rmPatterns) {
    object <- gsub(pattern = pattern, replacement = "",
                   x = object, fixed = TRUE)
  }
  
  
  # this one at last: patterns like it's or that's should already been removed!
  object <- gsub(pattern = "'s", replacement = " its",
                 x = object, fixed = TRUE )
 
  
  return(paste0(object, collapse = " "))
}