
library(quanteda.textplots)## Semantic network
library(quanteda)
library(RColorBrewer)
library(dplyr)


hashtags_network_hub <- function(df, hub, top_n = 40){
  for(i in 1:nrow(df)){
    df$hashtags[[i]] <- tolower(paste(paste0("#", gsub('[[:punct:]]', ' ',iconv(df$hashtags[[i]],from="UTF-8",to="ASCII//TRANSLIT"))), collapse = " "))
  }
  df$hashtags <- gsub("#na", NA, df$hashtags)
  df$hashtags <- gsub("# ", " ", df$hashtags)
  df <- df[!is.na(df$hashtags),]

  #df$hashtags <- iconv(df$hashtags,from="UTF-8",to="ASCII//TRANSLIT")
  #df$hashtags <- gsub('[[:punct:]]', ' ', df$hashtags)


  hash_dfm <- tokens(df$hashtags) # Document-feature matrix

  fcmat <- fcm(hash_dfm , context = "window", tri = FALSE) # Feature-occurance matrix which shows the network structure

  ##selecting keywords inside fcmat
  tmp <- fcmat[, hub]
  tmp <- convert(tmp, to = "data.frame")
  colnames(tmp) <- c("nodes", "degree")
  tmp <- tmp[tmp$degree > 0, ]

  nodes_degree <- tmp[tmp$nodes == hub, ]$degree
  high_nodes <- tmp[tmp$degree > nodes_degree, ]$nodes

  fcm_local <- fcm_select(fcmat, pattern = c(tmp, hub))
  fcm_local <- fcm_remove(fcm_local, high_nodes)

  ##reduce only top words
  feat <-  names(topfeatures(fcm_local, top_n))

  fcm_local <- fcm_select(fcmat, pattern = c(tmp, hub))

  ##text plot
  quanteda.textplots::textplot_network(fcm_local, min_freq = 0.1, edge_color = "grey",vertex_color ="#538797")
}









