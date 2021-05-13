library(dplyr)
keyword_table <- function(df, keyword_remove, top_n = 10){
  #df$text <- gsub("@\\w*", "", df$text)
  #df$text <- gsub("#\\w*", "", df$text)
  toks <- tokens(df$text, remove_punct = TRUE, remove_symbols = TRUE, verbose = TRUE)
  toks <- tokens_tolower(toks)
  toks <- tokens_remove(toks, padding = FALSE, min_nchar =3)
  hash_dfm <- dfm(toks) # Document-feature matrix
  hash_dfm <- dfm_remove(hash_dfm, keyword_remove)
  toptag <- sort(topfeatures(hash_dfm, top_n), decreasing = TRUE)
  toptag <- data.frame(toptag)
  toptag <- tibble::rownames_to_column(toptag, "keyword")
  colnames(toptag) <- c("keyword", "freq")
  #print(kable(toptag, "simple", caption = paste("Top", top_n, "keyword")))
  return(toptag)
}
