
library(quanteda.textplots)## Semantic network
library(quanteda)
library(RColorBrewer)
library(dplyr)


mention_network <- function(df, n_top = 50){

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

 toptag <- names(topfeatures(hash_dfm, n_top)) # Most important mentions_screen_name; we dont want to plot every hashtag
 tag_fcm <- fcm(hash_dfm) # Feature-occurance matrix which shows the network structure
 topgat_fcm <- fcm_select(tag_fcm, pattern = toptag) # Filter results so that we plot only 50 top mentions_screen_name

 ##draw semantic network plot
 quanteda::textplot_network(topgat_fcm, min_freq = 1, edge_color = "grey",vertex_color ="#538797")
}









