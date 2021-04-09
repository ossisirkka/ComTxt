## Semantic network

library(quanteda)
library(RColorBrewer)
library(dplyr)


keyword_network <- function(df, keyword_remove, n_top = 40){
  df$text <- gsub("@\\w*", "", df$text)
  df$text <- gsub("#\\w*", "", df$text)
  toks <- tokens(df$text, remove_punct = TRUE, remove_symbols = TRUE, verbose = TRUE)
  toks <- tokens_tolower(toks)
  toks <- tokens_remove(toks, padding = FALSE, min_nchar =3)
  #toks <- tokens_wordstem(toks, language = language)

  ## A feature co-occurrence matrix
  fcmat <-quanteda::fcm(toks, context = "window", tri = FALSE)

  ##reduce only top words
  feat <-  names(topfeatures(fcmat, n_top))

  ##subset top 40 words
  fcm_1 <- fcm_select(fcmat, pattern = feat)
  fcm_1 <- fcm_remove(fcm_1, keyword_remove)
  ##draw semantic network plot
  quanteda::textplot_network(fcm_1, min_freq = 1, edge_color = "grey",vertex_color ="#538797")
}









