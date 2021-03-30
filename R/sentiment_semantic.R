## Semantic network for anger and fear sadness---------------------
library(quanteda)
library(quanteda.textplots)

sentiment_semantic <- function(df, select = "surprise", n_nodes = 100){
  select_nrc <- df %>%
    filter(sentiment %in% select)

  user_data <- unique(select_nrc$status_id)
  net <- list()
  for (i in 1:length(user_data)){
    if(length(select_nrc[select_nrc$status_id == user_data[i],]$word)>1){
      word <- paste(unique(select_nrc[select_nrc$status_id == user_data[i],]$word), sep = "", collapse = " ")
    } else {
      word<- select_nrc[select_nrc$status_id == user_data[i],]$word
    }
    net[[i]] <- as.data.frame(cbind(id = user_data[i], word))
  }
  net_anger <- do.call(rbind, net)
  tweets_dfm <- dfm(net_anger$word) # Document-feature matrix
  #tag_dfm <- dfm_select(tweets_dfm, pattern = ("#*")) # Get hashtags
  toptag <- names(topfeatures(tweets_dfm, n_nodes)) # Most important hashtags; we dont want to plot every hashtag
  tag_fcm <- fcm(tweets_dfm) # Feature-occurance matrix which shows the network structure
  topgat_fcm <- fcm_select(tag_fcm, pattern = toptag) # Filter results so that we plot only 50 top hashtags
  textplot_network(topgat_fcm,  edge_alpha = 0.5, edge_size = 5, edge_color = "grey",vertex_color ="#538797")

}


sentiment_semantic(d, select = "anticipation", n_nodes = 200)



