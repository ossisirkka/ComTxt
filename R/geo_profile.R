##profile_geo
library(purrr)
library(dplyr)
library(maps)

geo_profile <- function(df, location_country,  multi_name){
## Seperating city and country with
tmp <- list()
for(i in 1:length(df$location)){
  tmp[[i]] <- unlist(strsplit(as.character(df$location[i]), ","))
}

location_1 <- list()
for(i in 1:length(tmp)){
  location_1[[i]] <- list(city = unlist(tmp[i])[1], country = unlist(tmp[i])[2])
}

#location_1 <- reduce(location_1, bind_rows)

#library (plyr)
#location_1 <- ldply(location_1, data.frame)
location_1 <- dplyr::bind_rows(location_1)
## location_tmp: seperating cities and country ()
tmp <- list()
for(i in 1:nrow(location_1)){
  tmp[[i]] <- unlist(strsplit(as.character(location_1[i, 1]), "\\(|\\)"))
}

location_tmp <- list()
for(i in 1:length(tmp)){
  location_tmp[[i]] <- list(city = unlist(tmp[i])[1], country = unlist(tmp[i])[2])
}


location_tmp <- dplyr::bind_rows(location_tmp)
#location_tmp <- reduce(location_tmp, bind_rows)

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

#location_tmp <- reduce(location_tmp, bind_rows)
location_tmp <- dplyr::bind_rows(location_tmp)
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

#location_tmp <- reduce(location_tmp, bind_rows)
location_tmp <- dplyr::bind_rows(location_tmp)

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

#location_tmp <- reduce(location_tmp, bind_rows)
location_tmp <- dplyr::bind_rows(location_tmp)

##location_df :combining loctaion_df and location tmp
location_df <- cbind(location_tmp, location_df[,2])

for(i in 1:nrow(location_df)){
  if (is.na(location_df[[i,3]]) == FALSE){
    location_df[[i,2]] <- as.character(location_df[[i,3]])
  }
}

location_df <- location_df[,1:2]
location_df[,1] <- gsub("\\|..*", "", location_df[,1])##deleting multiple cities
location_df[,1] <- gsub("<u+.*", "", location_df[,1])##deleting unicode
location_df[,1] <- gsub("[0-9]+", "",location_df[,1], perl = T)
location_df[,2] <- gsub("[0-9]+", "",location_df[,2], perl = T)
location_df[,1] <- gsub("[[:punct:][:blank:]]+", " ", location_df[,1], perl = T)##deleting puntuation city
location_df[,2] <- gsub("[[:punct:][:blank:]]+", " ", location_df[,2], perl = T)##deleting puntuation country
location_df$country <- trimws(location_df$country)
##language specific
location_df[,2] <- tolower(location_df[,2])##lowercase
location_df[,1] <- tolower(location_df[,1])

colnames(location_df)<- c("city.x", "country.x")

location_df <-
  location_df %>%
  mutate(
    country.x = ifelse(location_df$city.x %in% c(multi_name, location_country), location_country, location_df$country.x)
  )

location_df <-
  location_df %>%
  mutate(
    country.x = ifelse(location_df$country.x %in% multi_name, location_country, location_df$country.x)
  )
## if cities names is spain add to country column----
location_df <- as.data.frame(location_df)
location_df <-
  location_df %>%
  mutate(
    country.x =ifelse(location_df[,1] == location_country, location_country, location_df[,2])
  )

location_df[,1] <- gsub("^\\s+|\\s+$", "", location_df[,1])##deleting space
location_df[,2] <- gsub("^\\s+|\\s+$", "", location_df[,2])##deleting space

data("world.cities")

## now lets find the cities cities
world.cities$country.etc <- tolower(world.cities$country.etc)
city_df <- world.cities[world.cities$country.etc == location_country, ]
city_df$name <- tolower(city_df$name)

location_df <-
  location_df %>%
  mutate(
    country.x = ifelse(location_df$city.x %in% city_df$name, location_country, location_df$country.x)
  )

#location_df <- cbind(location_df, city = df$city, country = df$country)
location_df$city.x <- iconv(location_df$city.x ,from="UTF-8",to="ASCII//TRANSLIT")

df <- subset(df, select = -c(location))
df <- data.frame(df, location_df)
df$city <- tolower(df$city)
df$city <-gsub('[[:punct:] ]+',' ',df$city)

df$city.x <-gsub('[[:punct:] ]+',' ',df$city.x)

df <-
  df %>%
  mutate(
    country.x = ifelse(str_detect(df$city.x, df$city) == TRUE, location_country, df$country.x)
  )

df <-
  df %>%
  mutate(
    country.x = ifelse(str_detect(df$city.x, location_country) == TRUE, location_country, df$country.x)
  )

df <-
  df %>%
  mutate(
    city.x = ifelse(str_detect(df$city.x, df$city) == TRUE, df$city, df$city.x)
  )

df <-
  df %>%
  mutate(
    for(i in 1:length(multi_name)){
    country.x = ifelse(str_detect(df$city.x, multi_name[[i]]) == TRUE, location_country, df$country.x)
    }
  )

df$country.x <- iconv(df$country.x ,from="UTF-8",to="ASCII//TRANSLIT")
df$country.x <- iconv(df$country.x ,from="UTF-8",to="ASCII//TRANSLIT")
df <-
  df %>%
  mutate(
    country.x = ifelse(df$country.x %in% city_df$name, location_country, df$country.x)
  )


df <- df[which(df$country.x == location_country),]


for(i in 1:nrow(df)){
  if(nrow(city_df[city_df$name == df$city.x[i],])==1){
    df$lat[[i]] <- city_df[city_df$name == df$city.x[i],]$lat
    df$lng[[i]] <- city_df[city_df$name == df$city.x[i],]$long
  }
  if(nrow(city_df[city_df$name == df$city.x[i],])==0) {
    df$lat[[i]] <- NA
    df$lng[[i]] <- NA
  }
}


##delete country name in city column
delete <- c(multi_name, location_country)
delete <-iconv(delete,from="Latin1",to="ASCII//TRANSLIT")
for(i in 1:length(delete)){
  df$city.x <- gsub(delete[i], NA, df$city.x)}


names(df)[names(df) == 'city'] <- 'place_city'
names(df)[names(df) == 'country'] <- 'place_country'
names(df)[names(df) == 'city.x'] <- 'city'
names(df)[names(df) == 'country.x'] <- 'country'

return(df)
}
