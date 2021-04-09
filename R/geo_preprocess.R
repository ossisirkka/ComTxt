library(purrr)
library(dplyr)
library(maps)
geo_preprocess <- function(df, location_country,  multi_name){


  ## Seperating city and country with ,
  tmp <- list()
  for(i in 1:length(df$location)){
    tmp[[i]] <- unlist(strsplit(as.character(df$location[i]), ","))
  }

  location_1 <- list()
  for(i in 1:length(tmp)){
    location_1[[i]] <- list(city = unlist(tmp[i])[1], country = unlist(tmp[i])[2])
  }

  location_1 <- reduce(location_1, bind_rows)

  ## location_tmp: seperating cities and country ()
  tmp <- list()
  for(i in 1:nrow(location_1)){
    tmp[[i]] <- unlist(strsplit(as.character(location_1[i, 1]), "\\(|\\)"))
  }

  location_tmp <- list()
  for(i in 1:length(tmp)){
    location_tmp[[i]] <- list(city = unlist(tmp[i])[1], country = unlist(tmp[i])[2])
  }

  location_tmp <- reduce(location_tmp, bind_rows)

  ## location_ df: combining location_1 data and location_tmp data adjusting contry coulmn
  location_df <- cbind(location_tmp, location_1[,2])

  for(i in 1:nrow(location_df)){
    if (is.na(location_df[[i,3]]) == FALSE){
      location_df[[i,2]] <- as.character(location_df[[i,3]])
    }
  }

  location_df <- location_df[,1:2]

  ## location_tmp: seperating city and country with -
  tmp <- list()
  for(i in 1:nrow(location_df)){
    tmp[[i]] <- unlist(strsplit(as.character(location_df[i, 1]), "[-]"))
  }

  location_tmp <- list()
  for(i in 1:length(tmp)){
    location_tmp[[i]] <- list(city = unlist(tmp[i])[1], country = unlist(tmp[i])[2])
  }

  location_tmp <- reduce(location_tmp, bind_rows)

  ## location_df: combining loctaion_df and location tmp

  location_df <- cbind(location_tmp, location_df[,2])

  for(i in 1:nrow(location_df)){
    if (is.na(location_df[[i,3]]) == FALSE){
      location_df[[i,2]] <- as.character(location_df[[i,3]])
    }
  }

  location_df <- location_df[,1:2]

  ##location_tmp: seperating city and country with _
  tmp <- list()
  for(i in 1:nrow(location_df)){
    tmp[[i]] <- unlist(strsplit(as.character(location_df[i, 1]), "[_]"))
  }

  location_tmp <- list()
  for(i in 1:length(tmp)){
    location_tmp[[i]] <- list(city = unlist(tmp[i])[1], country = unlist(tmp[i])[2])
  }

  location_tmp <- reduce(location_tmp, bind_rows)

  ## location_df: combining loctaion_df and location tmp
  location_df <- cbind(location_tmp, location_df[,2])

  for(i in 1:nrow(location_df)){
    if (is.na(location_df[[i,3]]) == FALSE){
      location_df[[i,2]] <- as.character(location_df[[i,3]])
    }
  }

  location_df <- location_df[,1:2]

  ## location_tmp : seperating cities "[.]"

  tmp <- list()
  for(i in 1:nrow(location_df)){
    tmp[[i]] <- unlist(strsplit(as.character(location_df[i, 1]), "[.]"))
  }

  location_tmp <- list()
  for(i in 1:length(tmp)){
    location_tmp[[i]] <-list(city = unlist(tmp[i])[1], country = unlist(tmp[i])[2])
  }

  location_tmp <- reduce(location_tmp, bind_rows)

  ##location_df :combining loctaion_df and location tmp
  location_df <- cbind(location_tmp, location_df[,2])

  for(i in 1:nrow(location_df)){
    if (is.na(location_df[[i,3]]) == FALSE){
      location_df[[i,2]] <- as.character(location_df[[i,3]])
    }
  }

  location_df <- location_df[,1:2]


  ## cleaning text -------
  location_df[,1] <- gsub("\\|..*", "", location_df[,1])##deleting multiple cities
  location_df[,1] <- gsub("<u+.*", "", location_df[,1])##deleting unicode
  location_df[,1] <- gsub("[0-9]+", "",location_df[,1], perl = T)
  location_df[,2] <- gsub("[0-9]+", "",location_df[,2], perl = T)
  location_df[,1] <- gsub("[[:punct:][:blank:]]+", " ", location_df[,1], perl = T)##deleting puntuation city
  location_df[,2] <- gsub("[[:punct:][:blank:]]+", " ", location_df[,2], perl = T)##deleting puntuation country
  location_df$country <- trimws(location_df$country)
  ##language specific
  location_df[,2] <- tolower(location_df[,2])##lowercase
  location_df[,1] <- tolower(location_df[,1])##lowercase

  location_df <-
    location_df %>%
    mutate(
      country = ifelse(location_df$city %in% multi_name, location_country, location_df$country)
    )

  location_df <-
    location_df %>%
    mutate(
      country = ifelse(location_df$country %in% multi_name, location_country, location_df$country)
    )

  ## if cities names is spain add to country column----
  location_df <- as.data.frame(location_df)
  location_df <-
    location_df %>%
    mutate(
      country =ifelse(location_df[,1] == location_country, location_country, location_df[,2])
    )

  location_df[,1] <- gsub("^\\s+|\\s+$", "", location_df[,1])##deleting space
  location_df[,2] <- gsub("^\\s+|\\s+$", "", location_df[,2])##deleting space

  ##geocode selecting
  geo <- list()
  for(i in 1:length(df$location)){
    if(is.na(df$geo_coords[[i]][[1]])){
      geo[[i]] <- list(lat = NA, lng = NA)
    } else {
      geo[[i]] <- list(lat = df$geo_coords[[i]][[1]], lng = df$geo_coords[[i]][[2]], country = location_country)
    }
  }
  geo <- reduce(geo, bind_rows)

  ##location_df :combining loctaion_df and location tmp
  location_df <- cbind(location_df, geo[,3])

  for(i in 1:nrow(location_df)){
    if (is.na(location_df[[i,3]]) == FALSE){
      location_df[[i,2]] <- as.character(location_df[[i,3]])
    }
  }

  location_df <- location_df[,1:2]

  data("world.cities")

  ## now lets find the cities cities
  world.cities$country.etc <- tolower(world.cities$country.etc)
  city_df <- world.cities[world.cities$country.etc == location_country, ]
  city_df$name <- tolower(city_df$name)

  location_df <-
    location_df %>%
    mutate(
      country = ifelse(location_df$city %in% city_df$name, location_country, location_df$country)
    )


  ## add to text_clean data fram
  df <- subset(df, select = -c(location, country))
  df <- data.frame(df, location_df)

  ## add lat lang from geo_coord
  df <- cbind(df, geo[,1:2])

  world.cities$country.etc <- tolower(world.cities$country.etc)

  city_df <- world.cities[world.cities$country.etc == location_country, ]
  city_df$name <- tolower(city_df$name)

  #geom <- list()
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

  df <- df[which(df$country == location_country),]


  return(df)
}
