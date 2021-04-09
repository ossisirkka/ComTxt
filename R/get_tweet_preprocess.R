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
  tmp$geo_coord <- x$data$geo$coordinates$coordinates
  tmp$context_annotations <- x$data$context_annotations
  tmp <- cbind(tmp, x$data$public_metrics)
  tmp.1 <- list()
  for (i in 1:nrow(x$data)){
    extended_url<- x$data$entities$urls[[i]][[4]]
    if(length(extended_url)==0){
      extended_url <- NA
    } else if (length(extended_url) > 1){
      extended_url <-paste0(extended_url, sep="", collapse=",")
    } else {
      extended_url <- extended_url
    }
    hastags <- x$data$entities$hashtags[[i]][[3]]
    hastags <-paste0(hastags, sep="", collapse=",")
    mention <- x$data$entities$mentions[[i]][[3]]
    if(length(mention)==0){
      mention <- NA
    } else if (length(mention) > 1){
      mention <-paste0(mention, sep="", collapse=",")
    } else {
      mention <- mention
    }
    tmp.1[[i]] <- data.frame(extended_url, hastags, mention)
  }
  tmp.1 <- do.call(rbind.data.frame, tmp.1)
  tmp <- cbind(tmp, tmp.1)
  ## from user data
  ## colname :[1] "public_metrics","protected" ,"name" ,"entities" , "location", "author_id", "url" ,"description" ,"profile_image_url", "created_at" ,"username" ,"verified","pinned_tweet_id"
  names(x$includes$users)[names(x$includes$users) == "id"] <- "author_id"
  df <- left_join(x$data, x$includes$users, by = "author_id")
  ## follower count, following count, tweet count, listed_count
  tmp$username <-df$username
  tmp <- cbind(tmp, df$public_metrics.y)
  tmp$profile_image_url <-df$profile_image_url
  tmp$name <-df$name
  tmp$location <-df$location
  tmp$description <-df$description
  tmp$profile_image_url <-df$profile_image_url
  tmp$created_at.y <-df$created_at.y
  tmp
})

df_tweets <- do.call(rbind.data.frame, tweets_list_clean)
colnames(df_tweets)[colnames(df_tweets) == 'id'] <- 'status_id'

## until Here ---------
for(i in 1:nrow(df)){
  if(length(df$geo_coord[[i]]) == 0){
    df$geo_coord[[i]][[1]] <- NA
    df$geo_coord[[i]][[2]] <- NA
  }
  dd <- df$geo_coord[[i]][1]
  df$geo_coord[[i]][[1]] <- df$geo_coord[[i]][[2]]
  df$geo_coord[[i]][[2]] <- dd
}


return(df_tweets)
}


