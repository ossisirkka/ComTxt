library(udpipe)
library(dplyr)
library(purrr)
library(dplyr)
library(maps)

get_tweet_preprocess <- function(list, location_country = "spain"){


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

  tmp$geo_coords <- if(length(x$data$geo$coordinates$coordinates)!=0){
    x$data$geo$coordinates$coordinates
  }else{
    list(NULL)
  }

  tmp$place_id <- x$data$geo$place_id
  tmp$context_annotations <- x$data$context_annotations

  tmp <- cbind(tmp, x$data$public_metrics)
  tmp.1 <- list()
  for (i in 1:nrow(x$data)){
    url_t.co <- x$data$entities$urls[[i]][["url"]]
    if(length(url_t.co)==0){
      url_t.co <- NA
    } else {
      url_t.co <- url_t.co
    }
    tmp.1[[i]] <- url_t.co
  }
  tmp$url_t.co <- tmp.1
  tmp.2 <- list()
  for (j in 1:nrow(x$data)){
    url_extended_url<- x$data$entities$urls[[j]][["expanded_url"]]
    if(length(url_extended_url)==0){
      url_extended_url <- NA
    } else {
      url_extended_url <- url_extended_url
    }
    tmp.2[[j]] <- url_extended_url
  }
  tmp$url_extended_url <- tmp.2

  tmp.3 <- list()
  for (k in 1:nrow(x$data)){
    hashtags <- x$data$entities$hashtags[[k]][["tag"]]
    if(length(hashtags) == 0){
      hashtags <- NA
    } else{
      hashtags <- hashtags
    }
    tmp.3[[k]] <- hashtags
  }
  tmp$hashtags <- tmp.3

  tmp.4 <- list()
  for (n in 1:nrow(x$data)){
    mentions_screen_name <- x$data$entities$mentions[[n]][["username"]]
    if(length(mentions_screen_name)==0){
      mentions_screen_name <- NA
    }  else {
      mentions_screen_name <- mentions_screen_name
    }
    tmp.4[[n]] <- mentions_screen_name
  }

  tmp$mentions_screen_name <- tmp.4
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

  tmp.5 <- list()
  for (m in 1:length(df$entities.y$url$urls)){
    profile_url <- df$entities.y$url$urls[[m]][["url"]]
    if(length(profile_url)==0){
      profile_url <- NA
    } else {
      profile_url <- profile_url
    }
    tmp.5[[m]] <- data.frame(profile_url)
  }
  tmp.5 <- do.call(rbind.data.frame, tmp.5)
  tmp$profile_url <- tmp.5

  names(x$includes$places)[names(x$includes$places) == "id"] <- "place_id"
  x$includes$places <- subset(x$includes$places, select = -geo )
  tmp <- left_join(tmp, x$includes$places, by = "place_id")

  colnames(tmp)[colnames(tmp) == 'id'] <- 'status_id'
  colnames(tmp)[colnames(tmp) == "author_id"] <- 'user_id'
  colnames(tmp)[colnames(tmp) == "following_count"] <- 'friends_count'
  colnames(tmp)[colnames(tmp) == "tweet_count"] <- 'statuses_count'
  colnames(tmp)[colnames(tmp) == "name.y"] <- 'city'
  colnames(tmp)[colnames(tmp) == "name.x"] <- 'name'

  col_order <- c("user_id", "status_id", "created_at","screen_name", "text",
                 "source", "conversation_id", "in_reply_to_user_id",
                 "mentions_screen_name","retweet_count","reply_count", "like_count",
                 "quote_count","hashtags","url_t.co", "url_extended_url","lang",
                 "geo_coords","place_id", "place_type","full_name", "country", "city" , "country_code" , "context_annotations","name","location", "description",
                 "followers_count","friends_count", "statuses_count",
                 "listed_count", "account_created_at","verified",
                 "profile_image_url", "profile_url")
  tmp <- tmp[col_order]

  tmp <- tmp[, col_order]

  tmp
})

df_tweets <- do.call(rbind.data.frame, tweets_list_clean)

#geom <- list()
#for(i in 1:nrow(df_tweets)){
#  if(length(df_tweets$geo_coords[[i]]) < 1){
#    df_tweets$geo_coords[[i]] <- c(NA, NA)
#  }
#  geom[[i]]<- c(df_tweets$geo_coords[[i]][[2]], df_tweets$geo_coords[[i]][[1]] )
#}

#df_tweets <-
#  df_tweets %>%
#  mutate(geo_coords = geom)

## now lets find the cities cities

world.cities$country.etc <- tolower(world.cities$country.etc)
city_df <- world.cities[world.cities$country.etc == location_country, ]

for(i in 1:nrow(df_tweets)){
  if(nrow(city_df[city_df$name == df_tweets$city[i],])==1){
    df_tweets$lat[[i]] <- city_df[city_df$name == df_tweets$city[i],]$lat
    df_tweets$lng[[i]] <- city_df[city_df$name == df_tweets$city[i],]$long
  }
  if(nrow(city_df[city_df$name == df_tweets$city[i],])==0) {
    df_tweets$lat[[i]] <- NA
    df_tweets$lng[[i]] <- NA
  }
}

return(df_tweets)
}


