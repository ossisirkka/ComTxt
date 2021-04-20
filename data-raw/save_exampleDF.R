library(stringr)
df$user_id <- str_replace(df$user_id, ".....$", "***")
df$screen_name <- str_replace(df$screen_name, "....$", "***")
df$reply_to_user_id <- str_replace(df$reply_to_user_id, "....$", "***")
df$reply_to_screen_name <- str_replace(df$reply_to_screen_name, "....$", "***")
df$retweet_user_id <- str_replace(df$retweet_user_id, "....$", "***")
df$retweet_screen_name <- str_replace(df$retweet_screen_name, "....$", "***")
df$retweet_user_id <- str_replace(df$retweet_user_id, "....$", "***")
df$name <- str_replace(df$name, ".......$", "***")

example_df <- df
save(example_df, file ="data/example_df.rda")


list_tweets <- list_raw[1:2]
list_tweets[1][[1]]$data$author_id <-str_replace(list_tweets[1][[1]]$data$author_id, ".....$", "***")

list_tweets[1][[1]]$data$in_reply_to_user_id <-str_replace(list_tweets[1][[1]]$data$in_reply_to_user_id, ".....$", "***")

list_tweets[1][[1]]$includes$users$name <-str_replace(list_tweets[1][[1]]$includes$users$name , ".....$", "***")

list_tweets[1][[1]]$includes$users$username <-str_replace(list_tweets[1][[1]]$includes$users$username , ".....$", "***")

save(list_tweets, file = "data/list_tweets.rda")


colnames(NRC_Emotion_Lexicon)[colnames(NRC_Emotion_Lexicon) == 'en...22'] <- 'en'
save(NRC_Emotion_Lexicon, file = "data/NRC_Emotion_Lexicon.rda")
