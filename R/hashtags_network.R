
library(quanteda.textplots)## Semantic network
library(quanteda)
library(RColorBrewer)
library(dplyr)
library(stringr)

hashtags_network <- function(df, hashtag_remove = "#cultura", top_n = 40){

  for(i in 1:nrow(df)){
    df$hashtags[[i]] <- tolower(paste(paste0("#", df$hashtags[[i]]), collapse = " "))
  }
  df$hashtags <- gsub("#na", NA, df$hashtags)
  df$hashtags <- iconv(df$hashtags,from="UTF-8",to="ASCII//TRANSLIT")
  df$hashtags <- stringr::str_remove_all(df$hashtag, "[\\p{P}\\p{S}&&[^#]]")
  df <- df[!is.na(df$hashtags),]
  hash_dfm <- dfm(df$hashtags)
  hash_dfm <- dfm_remove(hash_dfm, hashtag_remove)# Document-feature matrix
  toptag <- names(topfeatures(hash_dfm, top_n)) # Most important hashtags; we dont want to plot every hashtag
  tag_fcm <- fcm(hash_dfm) # Feature-occurance matrix which shows the network structure
  topgat_fcm <- fcm_select(tag_fcm, pattern = toptag) # Filter results so that we plot only 50 top hashtags

  ##draw semantic network plot
  quanteda.textplots::textplot_network(topgat_fcm, min_freq = 1, edge_color = "grey",vertex_color ="#538797")
}


#size =0.015
#, vertex_labelsize = rowSums(topgat_fcm)*size



