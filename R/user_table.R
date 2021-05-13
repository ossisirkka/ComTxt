
library(tidyverse)
library(dplyr)
user_table <- function(df, top_n){

  tmp <- df %>% select(user_id, screen_name, verified ,followers_count, friends_count, statuses_count)

  tmp <-
    tmp %>% distinct(user_id, .keep_all= TRUE)

  tmp$verified <- gsub(FALSE, "NO", tmp$verified)
  tmp$verified <- gsub(TRUE, "YES", tmp$verified)

  tmp_1 <- tmp %>%
    select(screen_name, verified ,followers_count) %>%
    arrange(desc(followers_count)) %>%
    head(top_n)

  tmp_2 <- tmp %>%
    select(screen_name, verified ,friends_count) %>%
    arrange(desc(friends_count)) %>%
    head(top_n)

  tmp_3 <- tmp %>%
    select(screen_name, verified ,statuses_count) %>%
    arrange(desc(statuses_count)) %>%
    head(top_n)

  tmp_4 <- as.data.frame(sort(table(df$screen_name), decreasing=TRUE))
  colnames(tmp_4 ) <- "N.tweet_culture"

  tmp <- cbind(tmp_1, tmp_2, tmp_3, tmp_4)
  print(kable(tmp, "simple", caption = paste("Top", top_n, "Users in follower, following, tweets number, N tweets about culture")))

}
