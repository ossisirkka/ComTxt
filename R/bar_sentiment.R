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
library(ggraph)


## sentiment of bar chart per year :: NEED TO PREPROCESS PER MONTH/ YEAR
bar_sentiment <- function(df, by = "%m%Y"){

  df$created_at <- format(df$created_at, by)


  df %>%
    group_by(created_at, sentiment) %>%
    summarise(word_count = n()) %>%
    ungroup() %>%
    mutate(sentiment = reorder(sentiment, word_count)) %>%
    ggplot(aes(sentiment, word_count, fill = sentiment)) +
    geom_col() +
    facet_wrap(~created_at)+
    labs(x = "sentiment" , y = "Word Count") +
    ggtitle("NRC Sentiment")+
    theme(legend.position = "none")

}

