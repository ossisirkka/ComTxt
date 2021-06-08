##### Packages you need to install/load ####


library(maps)
library(ggmap)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(stringr)

##### Parameters of the function tweets_map(country, df_2019, df_2020)
#country: country name in English: uk, spain, france, netherlands, finland, switzerland, serbia, croatia, denmark
#df_2019: Data frame produced with topic_preprocess() for 2019
#df_2020: Data frame produced with topic_preprocess() for 2020


#### Function ####
tweets_map <- function(country, df_2019, df_2020) {
  # get a map the country
  mapdata <-  map_data(map = "world", region = country)

    # 2019

  # data frame with the lat-long coordinates
  maptwt19 <- data.frame( long = as.numeric(as.vector(unlist(df_2019$lng))),
                          lat = as.numeric(as.vector(unlist(df_2019$lat))))

  # Delete NA
  maptwt19 <- maptwt19 %>% filter(!is.na(long) | !is.na(lat))

  # Create a varibal crossing lat and long
  maptwt19$conc <- paste(round(maptwt19$lat,3), round(maptwt19$long,3), sep ="_")

  # Create a new table coodinates vs number of tweets
  tmp19 <- maptwt19 %>% group_by(conc) %>% summarise( value = n())

  # Extract lat and long
  tmp19$lat <- as.numeric(str_extract(tmp19$conc, "^\\-?[:digit:]+\\.?[:digit:]+"))

  tmp19$long <- as.numeric(str_remove(str_extract(tmp19$conc, "\\_\\-?[:digit:]+\\.?[:digit:]+"), "\\_"))


  # 2020

  # data frame with the lat-long coordinates

  maptwt20 <- data.frame( long = as.numeric(as.vector(unlist(df_2020$lng))),
                          lat = as.numeric(as.vector(unlist(df_2020$lat))))
  # Delete NA

  maptwt20 <- maptwt20 %>% filter(!is.na(long) | !is.na(lat))

  # Create a varibal crossing lat and long
  maptwt20$conc <- paste(round(maptwt20$lat,3), round(maptwt20$long,3), sep ="_")

  # Create a new table coodinates vs number of tweets
  tmp20 <- maptwt20 %>% group_by(conc) %>% summarise( value = n())


  # Extract lat and long
  tmp20$lat <- as.numeric(str_extract(tmp20$conc, "^\\-?[:digit:]+\\.?[:digit:]+"))

  tmp20$long <- as.numeric(str_remove(str_extract(tmp20$conc, "\\_\\-?[:digit:]+\\.?[:digit:]+"), "\\_"))

  # correct the table dimensions
  if(dim(tmp19)[1] < dim(mapdata)[1]){
    tmp20[(dim(tmp19)[1]+1):dim(mapdata)[1],] <- NA

  } else{
    mapdata[(dim(mapdata)[1]+1):dim(tmp19)[1],] <- NA

  }



  if(dim(tmp20)[1] < dim(mapdata)[1]){
    tmp20[(dim(tmp20)[1]+1):dim(mapdata)[1],] <- NA

  } else{
    mapdata[(dim(mapdata)[1]+1):dim(tmp20)[1],] <- NA

  }


  # Create a variable weights

  tmp19$wgt <- 1.5+log(10e2*(tmp19$value/max(c(max(tmp19$value, na.rm = TRUE), max(tmp20$value, na.rm = TRUE)))))
  tmp20$wgt <- 1.5+log(10e2*(tmp20$value/max(c(max(tmp19$value, na.rm = TRUE), max(tmp20$value, na.rm = TRUE)))))


  graph <- ggplot(mapdata, aes(x = long, y = lat))+
    geom_polygon(aes(group = group), fill = "#E8E8E8")+
    geom_point(aes(tmp19$long, tmp19$lat, color = "2019"), size = tmp19$wgt, alpha = 0.5)+
    geom_point(aes(tmp20$long, tmp20$lat, color = "2020"), size = tmp20$wgt, alpha = 0.5)+
    theme_bw() +
    theme(text=element_text(size=12, family="Garamond", hjust = 0.5, vjust = 0.5),
          legend.title = element_text(colour = "Black",  face = "bold", size = 10), legend.position = "bottom")+
    scale_y_continuous("", breaks = NULL, labels = NULL)+
    scale_x_continuous("", breaks = NULL, labels = NULL)+
    scale_color_manual(values = c("#3366FF", "#f74444"), label = c("2019", "2020"))+
    guides(color = guide_legend(title="Year",
                                title.position = "top",
                                title.hjust = 0.5))
  print(graph)
}


#, limits = c(35,44)
#, limits = c(-10, 5)



