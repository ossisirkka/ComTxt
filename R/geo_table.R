## most tweeted cities
library(kableExtra)

geo_table <- function(df){
  tmp <- sort(table(df$city), decreasing = TRUE)
  tmp <- data.frame(tmp)
  colnames(tmp) <- c("city", "freq")
  rownames(tmp) <- NULL
  tmp <- tmp[order(-tmp$freq),]
  print(kable(head(tmp, 10), "simple", caption = "Top 10 locations"))
  #tmp <- head(tmp, 10)
  #return(tmp)
}

