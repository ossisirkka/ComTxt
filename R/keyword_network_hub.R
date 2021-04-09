## local hub


library(quanteda)
library(RColorBrewer)
library(dplyr)

keyword_network_hub <- function(df, hub, n_top){
  df$text <- gsub("@\\w*", "", df$text)
  df$text <- gsub("#\\w*", "", df$text)

  toks <- tokens(df$text, remove_punct = TRUE, remove_symbols = TRUE, verbose = TRUE)
  toks <- tokens_tolower(toks)
  toks <- tokens_remove(toks, padding = FALSE, min_nchar =3)


  ## A feature co-occurrence matrix
  fcmat <-quanteda::fcm(toks, context = "window", tri = FALSE)

  ##selecting keywords inside fcmat
  tmp <- fcmat[, hub]
  tmp <- convert(tmp, to = "data.frame")
  colnames(tmp) <- c("nodes", "degree")
  tmp <- tmp[tmp$degree > 0, ]

  nodes_degree <- tmp[tmp$nodes == hub, ]$degree
  high_nodes <- tmp[tmp$degree > nodes_degree, ]$nodes

  fcm_local <- fcm_select(fcmat, pattern = c(tmp, hub))
  fcm_local <- fcm_remove(fcm_local, high_nodes)

  ##reduce only top words
  feat <-  names(topfeatures(fcm_local, n_top))

  ##subset top 40 words
  fcm_local <- fcm_select(fcm_local, pattern = feat)
  ##text plot
  quanteda::textplot_network(fcm_local, min_freq = 0.1, edge_color = "grey",vertex_color ="#538797")
}





