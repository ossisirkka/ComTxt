##draw map

library(maps)
data("world.cities")

twitter_map <- function(df, location_country){
world.cities$country.etc <- tolower(world.cities$country.etc)

city_df <- world.cities[world.cities$country.etc == location_country, ]
city_df$name <- tolower(city_df$name)

for(i in 1:nrow(df)){
  if(nrow(city_df[city_df$name == df$city[i],])==1){
    df$lat[[i]] <- city_df[city_df$name == df$city[i],]$lat
    df$lng[[i]] <- city_df[city_df$name == df$city[i],]$long
  }
  if(nrow(city_df[city_df$name == df$city[i],])==0) {
    df$lat[[i]] <- NA
    df$lng[[i]] <- NA
  }
}


par(mar = c(0, 0, 0, 0))
maps::map('world', as.character(location_country))
title(paste("tweets in", location_country))
points(df$lng, df$lat, pch = 20, cex = 1, col = rgb(0, .3, .7, .75))
}
