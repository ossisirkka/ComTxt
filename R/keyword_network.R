## Semantic network

library(quanteda)
library(quanteda.textplots)
library(RColorBrewer)
library(dplyr)


keyword_network <- function(df, keyword_remove, top_n = 40){

  toks <- tokens(df$text, remove_punct = TRUE, remove_symbols = TRUE, verbose = TRUE)
  toks <- tokens_tolower(toks)
  toks <- tokens_remove(toks, padding = FALSE, min_nchar =3)
  #toks <- tokens_wordstem(toks, language = language)

  ## A feature co-occurrence matrix
  fcmat <-quanteda::fcm(toks, context = "window", tri = FALSE)

  ##reduce only top words
  feat <-  names(topfeatures(fcmat, top_n))

  ##subset top 40 words
  fcm_1 <- fcm_select(fcmat, pattern = feat)
  fcm_1 <- fcm_remove(fcm_1, keyword_remove)
  ##draw semantic network plot
  quanteda.textplots::textplot_network(fcm_1, min_freq = 1, edge_color = "grey",vertex_color ="#538797")
}




#size = 0.15
#, vertex_labelsize = rowSums(fcm_1)*size



