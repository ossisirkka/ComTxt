## Semantic network

library(quanteda)
library(RColorBrewer)
library(dplyr)


semantic_network <- function(df, language = "en", n_top = 40, vertext_size = 0.5){
  toks <- tokens(df$text, remove_punct = TRUE, remove_symbols = TRUE, verbose = TRUE)
  toks <- tokens_tolower(toks)
  toks <- tokens_remove(toks, pattern = stopwords::stopwords(language, source = "stopwords-iso"), padding = FALSE, min_nchar =3)
  toks <- tokens_wordstem(toks, language = language)

  ## A feature co-occurrence matrix
  fcmat <-quanteda::fcm(toks, context = "window", tri = FALSE)

  ##reduce only top words
  feat <-  names(topfeatures(fcmat, n_top))

  ##subset top 40 words
  fcm_1 <- fcm_select(fcmat, pattern = feat)
  ##draw semantic network plot
  quanteda::textplot_network(fcm_1, min_freq = 0.1, edge_color = "grey",vertex_color ="#538797",  vertex_labelsize = (rowSums(fcm_1)/min(rowSums(fcm_1)))*vertext_size, edge_size = 3)
}









