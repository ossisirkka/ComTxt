library(udpipe)
library(dplyr)
library(purrr)
library(dplyr)
library(maps)

get_tweet_preprocess <- function(list){
  tweets_list_clean <- lapply(list, function(x){
  ##from x$data
  selec_col <- c("created_at","id","text","lang","author_id", "source","conversation_id")
  tmp <- x$data[selec_col]
  for(i in 1:nrow(x$data)){
  if("in_reply_to_user_id" %in% colnames(x$data) == TRUE){
     in_reply_to_user_id <- x$data$in_reply_to_user_id
  }else{
    in_reply_to_user_id <- NA}
  }
  tmp$in_reply_to_user_id <- in_reply_to_user_id

  tmp$geo_coord <- x$data$geo$coordinates$coordinates
  tmp$context_annotations <- x$data$context_annotations
  tmp <- cbind(tmp, x$data$public_metrics)
  tmp.1 <- list()
  for (i in 1:nrow(x$data)){
    url_t.co <- x$data$entities$urls[[i]][[3]]
    if(length(url_t.co)==0){
      url_t.co <- NA
    } else {
      url_t.co <- url_t.co
    }
    tmp.1[[i]] <- url_t.co
  }
  tmp$url_t.co <- tmp.1
  tmp.1 <- list()
  for (i in 1:nrow(x$data)){
    url_extended_url<- x$data$entities$urls[[i]][[4]]
    if(length(url_extended_url)==0){
      url_extended_url <- NA
    } else {
      url_extended_url <- url_extended_url
    }
    tmp.1[[i]] <- url_extended_url
  }
  tmp$url_extended_url <- tmp.1

  tmp.1 <- list()
  for (i in 1:nrow(x$data)){
    hashtags <- x$data$entities$hashtags[[i]][[3]]
    if(length(hashtags) == 0){
      hashtags <- NA
    } else{
      hashtags <- hashtags
    }
    tmp.1[[i]] <- hashtags
  }
  tmp$hashtags <- tmp.1

  tmp.1 <- list()
  for (i in 1:nrow(x$data)){
    mentions_screen_name <- x$data$entities$mentions[[i]][[3]]
    if(length(mentions_screen_name)==0){
      mentions_screen_name <- NA
    }  else {
      mentions_screen_name <- mentions_screen_name
    }
    tmp.1[[i]] <- mentions_screen_name
  }

  tmp$mentions_screen_name <- tmp.1
  ## from user data

  names(x$includes$users)[names(x$includes$users) == "id"] <- "author_id"
  df <- left_join(x$data, x$includes$users, by = "author_id")
  ## follower count, following count, tweet count, listed_count
  tmp$screen_name <-df$username
  tmp <- cbind(tmp, df$public_metrics.y)
  tmp$profile_image_url <-df$profile_image_url
  tmp$name <-df$name
  tmp$location <-df$location
  tmp$description <-df$description
  tmp$verified <-df$verified
  tmp$profile_image_url <-df$profile_image_url
  tmp$account_created_at <-df$created_at.y

  tmp.2 <- list()
  for (i in 1:length(df$entities.y$url$urls)){
    profile_url <- df$entities.y$url$urls[[1]][[4]]
    if(length(profile_url)==0){
      profile_url <- NA
    } else {
      profile_url <- profile_url
    }
    tmp.2[[i]] <- data.frame(profile_url)
  }
  tmp.2 <- do.call(rbind.data.frame, tmp.2)
  tmp$profile_url <- tmp.2
  tmp
})

df_tweets <- do.call(rbind.data.frame, tweets_list_clean)
colnames(df_tweets)[colnames(df_tweets) == 'id'] <- 'status_id'
colnames(df_tweets)[colnames(df_tweets) == "author_id"] <- 'user_id'
## until Here ---------
geom <- list()
for(i in 1:nrow(df_tweets)){
  if(length(df_tweets$geo_coord[[i]]) < 1){
    df_tweets$geo_coord[[i]] <- c(NA, NA)
  }
  geom[[i]]<- c(df_tweets$geo_coord[[i]][[2]], df_tweets$geo_coord[[i]][[1]] )
}

df_tweets <-
  df_tweets %>%
  mutate(geo_coord = geom)

col_order <- c("user_id", "status_id", "created_at","screen_name", "text",
               "source", "conversation_id", "in_reply_to_user_id",
               "mentions_screen_name","retweet_count","reply_count", "like_count",
               "quote_count","hashtags","url_t.co", "url_extended_url","lang",
               "geo_coord","context_annotations","name","location", "description",
               "followers_count","following_count", "tweet_count",
               "listed_count", "account_created_at","verified",
               "profile_image_url", "profile_url")
df_tweets <- df_tweets[, col_order]
df_tweets
return(df_tweets)
}


