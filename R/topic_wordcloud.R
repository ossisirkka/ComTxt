
library(RColorBrewer)
library(wordcloud)
topic_wordcloud <- function(mallet_df, par_row = ceiling(sqrt(mallet_df$model$numTopic)), par_col = ceiling(sqrt(mallet_df$model$numTopic)),
                            n_topic =7,maximum_n = 200, cloud_scale = 2.5){
  pal2 <- brewer.pal(8,"Dark2")
  par(mfrow = c(par_row, par_col),     # 2x2 layout
      oma = c(2, 2, 0, 0), # two rows of text at the outer left and bottom margin
      mar = c(1, 1, 0, 0), # space for one row of text at ticks and to separate plots
      mgp = c(2, 1, 0),    # axis label at 2 rows distance, tick labels at 1 row
      xpd = NA)
  topic.words <- mallet.topic.words(mallet_df, smoothed = T, normalized = T)
  mallet_words_list <- list()
  for (i in 1:as.numeric(n_topic)) {
    mallet_words_list[[i]] <- mallet.top.words(mallet_df, topic.words[i,], maximum_n)
  }
  topic_mallet_list <- mallet_words_list

  for(i in 1:length(topic_mallet_list)){
    df_words <- as.data.frame(topic_mallet_list[i])
    wordcloud(df_words$words, df_words$weights, scale = c(cloud_scale,.2),
              max.words = maximum_n, random.order = FALSE, rot.per = .15, colors = pal2, main = "Mallet wordcloud")
  }
}


