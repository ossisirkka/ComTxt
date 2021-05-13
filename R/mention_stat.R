library(dplyr)
library(kableExtra)
mention_stat <- function(df){

  mention_df <- df$mentions_screen_name
  na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }
  ddd <-na.omit.list(mention_df)
  mention_percentage <- paste0(round(length(ddd)/nrow(df)*100, digits = 0), "%")

  cal_max <- list()
  for(i in 1:length(ddd)){
    cal_max[[i]] <- length(ddd[[i]])
  }
  cal_max <- unlist(cal_max)
  mean_n_mention <-round(mean(cal_max), digits = 1)

  check <- data.frame(mention_percentage = mention_percentage, mean_n_mention= mean_n_mention)

  return(check)
}
