hashtag_stat <- function(df) {
  hashtag_df <- df$hashtags
  na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }
  ddd <-na.omit.list(hashtag_df)
  hashtags_percentage <- paste0(round(length(ddd)/nrow(df)*100, digits = 0), "%")


  ## maximum number of hashtags
  cal_max <- list()
  for(i in 1:length(ddd)){
    cal_max[[i]] <- length(ddd[[i]])
  }
  cal_max <- unlist(cal_max)
  max_n_hashtags <- max(cal_max)
  mean_n_hashtags <-round(mean(cal_max), digits = 1)

  ## unique hashtags
  dddd <-unlist(ddd)
  total_hashtags<- length(dddd)
  dddd <- tolower(dddd)
  unique_hashtags<- length(unique(dddd))

  check <- data.frame(total_hashtags = total_hashtags, unique_hashtags = unique_hashtags, hashtags_percentage = hashtags_percentage, max_n_hashtags = max_n_hashtags, mean_n_hashtags= mean_n_hashtags)
  rownames(check) <- NULL
  print(kable(check, "simple", caption = "hashtags statistics"))
}


