given_model_K <- function(df, mallet_df){
  set.seed(12345)
  k = mallet_df$model$numTopics
  ## by perperxity
  dtm <- DocumentTermMatrix(Corpus(VectorSource(df$text)))

  #Filtering empty rows of the matrix (DTM) - important for performance
  rowTotals <- slam::row_sums(dtm)#apply(dtm, 1, sum)
  dtm_filter <- dtm[rowTotals > 0,]

  #Model optimization - calculating perplexity for different values of k
  set.seed(12345)
  train = sample(rownames(dtm_filter), nrow(dtm_filter) * .75)
  #train = 2000
  dtm_train = dtm_filter[rownames(dtm_filter) %in% train, ]
  dtm_test = dtm_filter[!rownames(dtm_filter) %in% train, ]

  m = LDA(dtm_train, method = "Gibbs", k = k,  control = list(alpha = 5/k))
  p = perplexity(m, dtm_test)

  best_K <- data.frame(type = "perplexity", score = p)
  return(best_K)
  ##create DTM
  #dtm <- CreateDtm(df$text,
  #                 doc_names = df$status_id,
  #                 ngram_window = c(1, 2))

  # m <- FitLdaModel(dtm = dtm, k = k, iterations = 2000)
  #m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)

  #d_1 <- data.frame(type = "perplexity", score = p)
  #d_2 <- data.frame(type = "coherence", score = m$coherence)
  #best_K <- rbind(d_1, d_2)
}
