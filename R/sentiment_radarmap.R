library(dplyr) #Data manipulation (also included in the tidyverse package)
library(tidytext) #Text mining
library(tidyr) #Spread, separate, unite, text mining (also included in the tidyverse package)
library(widyr) #Use for pairwise correlation
library(textdata)
#Visualizations!
library(ggplot2) #Visualizations (also included in the tidyverse package)
library(ggrepel) #`geom_label_repel`
library(gridExtra) #`grid.arrange()` for multi-graphs
library(knitr) #Create nicely formatted output tables
library(kableExtra) #Create nicely formatted output tables
library(formattable) #For the color_tile function
library(circlize) #Visualizations - chord diagram
library(memery) #Memes - images with plots
library(magick) #Memes - images with plots (image_read)
library(yarrr)  #Pirate plot
library(radarchart) #Visualizations
library(igraph) #ngram network diagrams
library(ggraph) #ngram network diagrams

## sentiment radarmap ------------------------------
sentiment_radarmap <- function(df){
  df$created_at <- format(df$created_at, "%Y")

  prince_nrc_sub <- df %>%
    filter(!sentiment %in% c("positive", "negative"))

  year_sentiment_nrc <- prince_nrc_sub %>%
    group_by(created_at, sentiment) %>%
    count(created_at, sentiment) %>%
    select(created_at, sentiment, sentiment_year_count = n)

  #Get the total count of sentiment words per video_name (not distinct)
  total_sentiment_video <- prince_nrc_sub %>%
    count(created_at) %>%
    select(created_at, year_total = n)

  col <- col2rgb(c("peachpuff", "royalblue", "tomato", "#B4CF68", "green", "purple", "orange","grey","red","#8DD3C7" , "pink", "blue","gold"))
  #Join the two and create a percent field
  year_radar_chart <- year_sentiment_nrc %>%
    inner_join(total_sentiment_video, by = "created_at") %>%
    mutate(percent = sentiment_year_count / year_total * 100 ) %>%
    #filter(year %in% c("1978","1994","1995")) %>%
    select(-sentiment_year_count, -year_total) %>%
    spread(created_at, percent) %>%
    chartJSRadar(showToolTipLabel = TRUE,
                 main = "NRC video Radar", colMatrix =col)
  print(year_radar_chart)

}

