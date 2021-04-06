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

## circos map -----------------------------------------------------
sentiment_circos <- function(df){
  df$created_at <- format(df$created_at, "%Y")
  my_colors <- c(brewer.pal(8, "Accent"), brewer.pal(8, "Dark2"), brewer.pal(12, "Paired"))

  #grid.col = c("02/2021" = my_colors[1], "01/2021" = my_colors[2], "12/2020" = my_colors[3], "11/2020" = my_colors[4], "10/2020" = my_colors[5], "9/2020" = my_colors[6],"8/2020" = my_colors[7],"7/2020" = my_colors[8],"6/2020" = my_colors[9],"5/2020" = my_colors[10],"4/2020" = my_colors[11],"3/2020" = my_colors[12], "2/2020" = my_colors[13],"1/2020" = my_colors[14],"12/2019" = my_colors[15],"11/2019" = my_colors[16],"10/2019" = my_colors[17],"9/2019" = my_colors[18],"8/2019" = my_colors[19],"7/2019" = my_colors[20],"6/2019" = my_colors[21],"5/2019" = my_colors[22],"4/2019" = my_colors[23],"3/2019" = my_colors[24],"2/2019" = my_colors[25],"1/2019" = my_colors[26],"anger" = "grey", "anticipation" = "grey", "disgust" = "grey", "fear" = "grey", "joy" = "grey", "sadness" = "grey", "surprise" = "grey", "trust" = "grey")

  my_colors <- c(brewer.pal(8, "Accent"), brewer.pal(8, "Dark2"), brewer.pal(12, "Paired"))
  grid.col = c("2021" = my_colors[1], "2020" = my_colors[2], "2019" = my_colors[3], "anger" = "grey", "anticipation" = "grey", "disgust" = "grey", "fear" = "grey", "joy" = "grey", "sadness" = "grey", "surprise" = "grey", "trust" = "grey")

  decade_mood <-  df %>%
    filter(!sentiment %in% c("positive", "negative")) %>%
    count(sentiment, created_at) %>%
    group_by(created_at, sentiment) %>%
    summarise(sentiment_sum = sum(n)) %>%
    ungroup()

  circos.clear()
  #Set the gap size
  circos.par(gap.after = c(rep(5, length(unique(decade_mood[[1]])) - 1), 15,
                           rep(5, length(unique(decade_mood[[2]])) - 1), 15))
  chordDiagram(decade_mood, grid.col = grid.col, transparency = .2,annotationTrack = c("grid", "axis"))
  circos.track(track.index = 1, panel.fun = function(x, y) {
    circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index,
                facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.3))
  }, bg.border = NA)
  title("Relationship Between Mood and Years")
}


