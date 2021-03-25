library(rlist)
library(kableExtra)
##words in topic
topic_words <- function(mallet_df, n_topic =7, n_words = 500){
  topic.words <- mallet.topic.words(mallet_df, smoothed = T, normalized = T)
  mallet_words_list <- list()
  for (i in 1:as.numeric(n_topic)) {
     mallet_words_list[[i]] <- mallet.top.words(mallet_df, topic.words[i,], n_words)
  }
  topic_mallet_list <- mallet_words_list

  topic_mallet <- list.cbind(topic_mallet_list)
  #kable(topic_mallet, "simple")
}

View(topic_words(topic, n_topic = 7, n_words = 100))

