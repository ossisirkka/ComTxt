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

## each words used in each video---------------------------------------
plot_words <- function(df){
  plot_words_UnderstandingLOU2 <- df %>%
    group_by(sentiment) %>%
    count(word, sort = TRUE) %>%
    arrange(desc(n)) %>%
    slice(seq_len(10)) %>%
    ungroup()

  plot_words_UnderstandingLOU2 %>%
    ggplot(aes(word, 1, label = word, fill = sentiment )) +
    geom_point(color = "transparent") +
    geom_label_repel(force = 1,nudge_y = .5,
                     direction = "y",
                     box.padding = 0.05,
                     segment.color = "transparent",
                     size = 3) +
    facet_grid(~sentiment) +

    theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
          axis.title.x = element_text(size = 6),
          panel.grid = element_blank(), panel.background = element_blank(),
          panel.border = element_rect("lightgray", fill = NA),
          strip.text.x = element_text(size = 9)) +
    xlab(NULL) + ylab(NULL) +
    ggtitle("NRC Sentiment words plot") +
    coord_flip()+
    theme(legend.position = "none")
}


plot_words(d)
