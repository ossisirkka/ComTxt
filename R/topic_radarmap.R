##topic_wordsplot
library(dplyr)
library(tidytext)
library(ggplot2)
library(scales)
library(mallet)
library(radarchart)
#create function that accepts the lda model and num word to display
topic_radarmap <- function(df ,mallet_df, n_topic = mallet_df$model$numTopics) {

  doc.topics.m <- mallet.doc.topics(mallet_df, smoothed=T,
                                    normalized=T)


  tmp_df <- df[rep(seq_len(nrow(df)), each = n_topic), ]
  #tmp_df$topic <- paste("Topic", rep(1:n_topic,  nrow(df)))

  tt <- list()
  for(i in 1:nrow(doc.topics.m)){
    tt[[i]] <- doc.topics.m[i,]
  }

  tmp_df$prob <- unlist(tt)
  tmp_df$topic <- paste("Topic", rep(1:n_topic,  nrow(df)))
  tmp_df$created_at <- format(as.Date(tmp_df$created_at), "%Y")

  year_prob <- aggregate(x = tmp_df$prob,                # Specify data column
                                      by = list(tmp_df$topic, tmp_df$created_at),              # Specify group indicator
                                      FUN = sum)

  year_topic <- tmp_df  %>%
    group_by(created_at, topic) %>%
    count(created_at, topic) %>%
    select(created_at, topic, year_topic_n = n)

  year_topic$prob <- year_prob$x

  year_topic$percent <- year_topic$prob / year_topic$year_topic_n *100

  col <- col2rgb(c("peachpuff", "royalblue", "tomato", "#B4CF68", "green", "purple", "orange","grey","red","#8DD3C7" , "pink", "blue","gold"))
  #Join the two and create a percent field
  year_radar_chart <- year_topic %>%
    select(-year_topic_n, -prob) %>%
    spread(created_at, percent) %>%
    chartJSRadar(showToolTipLabel = TRUE,
                 main = "Topic Radar", colMatrix =col)
  print(year_radar_chart)

}
