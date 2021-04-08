
semantic_get_hub <- function(df){
  toks <- tokens(df$text, remove_punct = TRUE, remove_symbols = TRUE, verbose = TRUE)
  toks <- tokens_tolower(toks)
  toks <- tokens_remove(toks, padding = FALSE, min_nchar =3)
  #toks <- tokens_wordstem(toks, language = language)

  ## A feature co-occurrence matrix
  fcmat <-quanteda::fcm(toks, context = "window", tri = FALSE)

  ##reduce only top words
  feat <-  names(topfeatures(fcmat, 20))

  ##subset top 40 words
  fcm_1 <- fcm_select(fcmat, pattern = feat)
  data_fcmat <- data.frame(name = rownames(fcmat))
  rownames(data_fcmat) <- NULL
  data_fcmat$degree <- rowSums(fcmat)
  table_fcmat <- data_fcmat
  ##see the tale
  fcm_tb <- table_fcmat[table_fcmat$name %in% feat, ]

  fcm_tb <- fcm_tb %>% arrange(desc(degree))

}
