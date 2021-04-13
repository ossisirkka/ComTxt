library(dplyr)
library(kableExtra)
mention_table <- function(df, top_n = 10){

  dd <- lapply(df$mentions_screen_name, data.frame)
  tmp <- list()
  for(i in 1:length(dd)){
    tmp[[i]] <- data.frame(from = df$screen_name[[i]], to = dd[[i]])
  }

  tmp <- do.call(rbind.data.frame, tmp)
  colnames(tmp) <- c("from", "to")
  df.network <- tmp
  df.network <-df.network[complete.cases(df.network), ]

  df.network$from <- paste0("@", df.network$from)
  df.network$to <- paste0("@", df.network$to)
  df.network$communication <- paste(df.network$from, df.network$to)


  hash_dfm <- dfm(df.network$communication)

  toptag <- topfeatures(hash_dfm, top_n)

  print(kable(as.table(toptag), "simple", caption = paste("Top", top_n, "mention_id")))
}
