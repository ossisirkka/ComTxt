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



undesirable_words <- c("holy shit", "bullshit", "fucking", "shit", "damn")

twitter_sentiment <- function(df){

  #Create tidy text format: Unnested, Unsummarized, -Undesirables, Stop and Short words
  nrc_tidy <- df %>%
  unnest_tokens(word, text) %>% #Break the lyrics into individual words
  filter(!word %in% undesirable_words) %>% #Remove undesirables
  filter(!nchar(word) < 3) %>% #Words like "ah" or "oo" used in music
  anti_join(stop_words)
  ## Nrc analysis
  youtube_nrc <- nrc_tidy %>%
  inner_join(get_sentiments("nrc"))
}


## sentiment of bar chart per year :: NEED TO PREPROCESS PER MONTH/ YEAR
bar_sentiment <- function(){

}


## divide shelby, mat, you, girlfriends, your, review, boyfriend
## in total sentiment scores
youtube_nrc %>%
  group_by(video_name, sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  #Use `fill = -word_count` to make the larger bars darker
  ggplot(aes(sentiment, word_count, fill = sentiment)) +
  geom_col() +
  facet_wrap(~video_name)+
  #guides(fill = FALSE)+ #Turn off the legend
  labs(x = "sentiment" , y = "Word Count") +
  theme(axis.text.x = element_blank())+
  ggtitle("NRC Sentiment")


## circos map -----------------------------------------------------

## this is about the relationship between sentiment score and video
my_colors <- c("#32064A", "#E56B1F", "#E42C6A", "#5F9EA0", "#3EB650", "#E12B38", "#EE7879","#334C89","#F4ABAA", "#CAE7DF", "#3778C2", "#FCD02C", "#B4CF68")

grid.col = c("Reddeadredemption2" = my_colors[1], "Godofwar" = my_colors[2], "Darksouls" = my_colors[3], "Zelda" = my_colors[4], "Thelastofus" = my_colors[5], "Thewitcher3" = my_colors[6],"Horizonzerodawn" = my_colors[7],"Sekiro" = my_colors[8],"Deathstranding" = my_colors[9],"Starwarsjedi" = my_colors[10],"Animalcrossing" = my_colors[11],"UnderstandingLOU2" = my_colors[12], "ReviewingLOU2" = my_colors[13],"anger" = "grey", "anticipation" = "grey", "disgust" = "grey", "fear" = "grey", "joy" = "grey", "sadness" = "grey", "surprise" = "grey", "trust" = "grey")

decade_mood <-  youtube_nrc %>%
  filter(video_name != "NA" & !sentiment %in% c("positive", "negative")) %>%
  count(sentiment, video_name) %>%
  group_by(video_name, sentiment) %>%
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
title("Relationship Between Mood and video")


## each words used in each video---------------------------------------
plot_words_UnderstandingLOU2 <- youtube_nrc %>%
  filter(video_name %in% c("UnderstandingLOU2","ReviewingLOU2")) %>%
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
  ggtitle("NRC Sentiment understanding lost of us 2 & reviewing lost of us 2") +
  coord_flip()


## sentiment radarmap ------------------------------
prince_nrc_sub <- youtube_nrc %>%
  filter(!sentiment %in% c("positive", "negative"))

video_sentiment_nrc <- prince_nrc_sub %>%
  group_by(video_name, sentiment) %>%
  count(video_name, sentiment) %>%
  select(video_name, sentiment, sentiment_video_count = n)

prince_nrc_sub <- nrc_shelby %>%
  filter(!sentiment %in% c("positive", "negative"))

youtuber_sentiment_nrc <- prince_nrc_sub %>%
  group_by(video_name, sentiment) %>%
  count(video_name, sentiment) %>%
  select(video_name, sentiment, sentiment_video_count = n)

#Get the total count of sentiment words per video_name (not distinct)
total_sentiment_video <- prince_nrc_sub %>%
  count(video_name) %>%
  select(video_name, video_name_total = n)

col <- col2rgb(c("peachpuff", "royalblue", "tomato", "#B4CF68", "green", "purple", "orange","grey","red","#8DD3C7" , "pink", "blue","gold"))
#Join the two and create a percent field
year_radar_chart <- video_sentiment_nrc %>%
  inner_join(total_sentiment_video, by = "video_name") %>%
  mutate(percent = sentiment_video_count / video_name_total * 100 ) %>%
  #filter(year %in% c("1978","1994","1995")) %>%
  select(-sentiment_video_count, -video_name_total) %>%
  spread(video_name, percent) %>%
  chartJSRadar(showToolTipLabel = TRUE,
               main = "NRC video Radar", colMatrix =col)
print(year_radar_chart)



## Semantic network for anger and fear sadness---------------------
angry_nrc <- nrc_shelby %>%
  filter(video_name %in% c("UnderstandingLOU2","ReviewingLOU2") & sentiment %in% "anger")

user_data <- unique(angry_nrc$authorDisplayName)
net <- list()
for (i in 1:length(user_data)){
  if(length(angry_nrc[angry_nrc$authorDisplayName == user_data[i],]$word)>1){
    word <- paste(unique(angry_nrc[angry_nrc$authorDisplayName == user_data[i],]$word), sep = "", collapse = " ")
  } else {
    word<- angry_nrc[angry_nrc$authorDisplayName == user_data[i],]$word
  }
  net[[i]] <- as.data.frame(cbind(id = user_data[i], word))
}
net_anger <- do.call(rbind, net)

library(quanteda)
library(quanteda.textplots)
tweets_dfm <- dfm(net_anger$word) # Document-feature matrix
#tag_dfm <- dfm_select(tweets_dfm, pattern = ("#*")) # Get hashtags
toptag <- names(topfeatures(tweets_dfm, 100)) # Most important hashtags; we dont want to plot every hashtag
tag_fcm <- fcm(tweets_dfm) # Feature-occurance matrix which shows the network structure
topgat_fcm <- fcm_select(tag_fcm, pattern = toptag) # Filter results so that we plot only 50 top hashtags
textplot_network(topgat_fcm, min_freq = 12, edge_alpha = 0.5, edge_size = 5, edge_color = "grey",vertex_color ="#538797")


