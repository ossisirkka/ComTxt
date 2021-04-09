
library(quanteda.textplots)## Semantic network
library(quanteda)
library(RColorBrewer)
library(dplyr)


hashtags_network_hub <- function(df, hashtag_remove, n_top = 40){
  for(i in 1:nrow(df)){
    df$hashtags[[i]] <- tolower(paste(paste0("#", df$hashtags[[i]]), collapse = " "))
  }
  df$hashtags <- gsub("#na", NA, df$hashtags)
  df <- df[!is.na(df$hashtags),]
  hash_dfm <- tokens(df$hashtags) # Document-feature matrix

  fcmat <- fcm(hash_dfm , context = "window", tri = FALSE) # Feature-occurance matrix which shows the network structure

  ##selecting keywords inside fcmat
  tmp <- fcmat[, hub]
  tmp <- convert(tmp, to = "data.frame")
  colnames(tmp) <- c("nodes", "degree")
  tmp <- tmp[tmp$degree > 0, ]

  fcm_local <- fcm_select(fcmat, pattern = c(tmp, hub))
  fcm_local <- fcm_remove(fcm_local, hashtag_remove)
  ##text plot
  quanteda::textplot_network(fcm_local, min_freq = 0.1, edge_color = "grey",vertex_color ="#538797")
}









