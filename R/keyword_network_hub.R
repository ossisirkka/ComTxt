## local hub


library(quanteda)
library(RColorBrewer)
library(dplyr)

keyword_network_hub <- function(df, keyword_remove,hub){
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

  fcm_local <- fcm_select(fcmat, pattern = c(tmp, hub))
  fcm_local <- fcm_remove(fcm_local, keyword_remove)
  ##text plot
  quanteda::textplot_network(fcm_local, min_freq = 0.1, edge_color = "grey",vertex_color ="#538797")
}





