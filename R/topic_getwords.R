library(rlist)
library(kableExtra)
library(mallet)
##words in topic
topic_getwords <- function(mallet_df, n_topic = mallet_df$model$numTopics, n_words = 500){
  topic.words <- mallet.topic.words(mallet_df, smoothed = T, normalized = T)
  mallet_words_list <- list()
  for (i in 1:as.numeric(n_topic)) {
     mallet_words_list[[i]] <- mallet.top.words(mallet_df, topic.words[i,], n_words)
  }
  topic_mallet_list <- mallet_words_list

  topic_mallet <- list.cbind(topic_mallet_list)
  print(kable(topic_mallet, "simple"))
}

