
hashtags_table <- function(df, hashtag_remove, top_n = 10){
  all_hashtags <- tolower(unlist(df$hashtags))
  all_hashtags <- paste0("#",all_hashtags)
  all_hashtags <- iconv(all_hashtags,from="UTF-8",to="ASCII//TRANSLIT")
  #all_hashtags <- gsub("#NA", "", all_hashtags)
  hashtag_frequencies <- sort(table(all_hashtags), decreasing = TRUE)
  hashtag_frequencies <- hashtag_frequencies[!row.names(hashtag_frequencies)%in% c(hashtag_remove, "#NA")]
  print(kable(head(hashtag_frequencies, top_n), "simple", caption = paste("Top", top_n, "hashtags")))
}
