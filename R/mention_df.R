library(dplyr)
mention_df <- function(df, top_n = 10){
  all_mention <- tolower(unlist(df$mention))
  all_mention <- paste0("@",all_mention)
  #all_mention <- gsub("#NA", "", all_mention)
  hashtag_frequencies <- sort(table(all_mention), decreasing = TRUE)
  hashtag_frequencies <- hashtag_frequencies[!row.names(hashtag_frequencies)%in% c(hashtag_remove, "#NA")]
  print(kable(head(hashtag_frequencies, top_n), "simple", caption = paste("Top", top_n, "mention_id")))
}
