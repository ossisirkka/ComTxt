## most tweeted cities
library(kableExtra)

geo_table <- function(df){
  hashtag_frequencies <- sort(table(df$city), decreasing = TRUE)
  colnames(tmp) <- c("city", "freq")
  rownames(tmp) <- NULL
  tmp <- tmp[order(-tmp$freq),]
  print(kable(head(tmp, 10), "simple", caption = "Top 10 locations"))
}

