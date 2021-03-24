## local hub


library(quanteda)
library(RColorBrewer)
library(dplyr)


semantic_network_hub <- function(){
  toks <- tokens(df$Answer, remove_punct = TRUE, remove_symbols = TRUE, verbose = TRUE)
  toks <- tokens_tolower(toks)
  toks <- tokens_remove(toks, pattern = stopwords::stopwords(language, source = "stopwords-iso"), padding = FALSE, min_nchar =3)
  toks <- tokens_wordstem(toks, language = language)

  ## A feature co-occurrence matrix
  fcmat <-quanteda::fcm(toks, context = "window", tri = FALSE)

  ##selecting keywords inside fcmat
  tmp <- fcmat[, "art"]
  tmp <- as.data.table(tmp)
  colnames(tmp) <- c("nodes", "degree")
  tmp <- tmp[tmp$degree > 0, ]

  fcm_local <- fcm_select(fcmat, pattern = c(tmp, "art"))
  ##text plot
  quanteda::textplot_network(fcm_local, min_freq = 0.1, edge_color = "grey",vertex_color ="#538797",  vertex_labelsize = (rowSums(fcm_local)/min(rowSums(fcm_local)))*0.3, edge_size = 3)
}





