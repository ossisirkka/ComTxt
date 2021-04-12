##topic_wordsplot
library(dplyr)
library(tidytext)
library(ggplot2)

#create function that accepts the lda model and num word to display
topic_wordplot <- function(mallet_df, k, num_words = 10) {

  doc.topics.m <- mallet.doc.topics(mallet_df, smoothed=T,
                                    normalized=T)

  ddd <- data.frame(prob = colSums(doc.topics.m))
  ddd$topic <- 1:7

  ggplot(ddd, aes(x="", y=prob, fill=as.factor(topic))) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    theme_void() +
    #theme(legend.position="none") +
    geom_text(aes(y =cumsum(prob) - 1.0 *prob,
                  label = percent(prob/100)), size=5)
    #geom_text(aes(y = prob/10 + c(0, cumsum(prob)[-length(prob)]), label = percent(prob/100)), color = "white", size=6)
    #scale_fill_brewer(palette="Set1")

  df_15 <- sub_df[sub_df$created_at == "15/02/2021",]
  colSums(df_15[,4:10])/nrow(df_15)*100
  ## ntil here I do ----------------------------

  colSums(doc.topics.m)

  dd <- df[rep(seq_len(nrow(df)), each = 7), ]
  dd$topic <- rep(1:k,  100)

  tt <- list()
  for(i in 1:nrow(doc.topics.m)){
    tt[[i]] <- doc.topics.m[i,]
  }

  dd$prob <- unlist(tt)

  dd$created_at <- format(dd$created_at, "%d/%m/%Y")

  sub_df <- df %>%
    mutate(created_at = format(df$created_at, "%d/%m/%Y")) %>%
    select(created_at, status_id, text)


  sub_df <- cbind(sub_df, doc.topics.m)

  year_topic <- dd %>%
    group_by(created_at, topic) %>%
    select(created_at, topic, prob)

  total_sentiment_video <- prince_nrc_sub %>%
    count(created_at) %>%
    select(created_at, year_total = n)


  col <- col2rgb(c("peachpuff", "royalblue", "tomato", "#B4CF68", "green", "purple", "orange","grey","red","#8DD3C7" , "pink", "blue","gold"))
  #Join the two and create a percent field
  year_radar_chart <- year_topic %>%
    inner_join(total_sentiment_video, by = "created_at") %>%
    mutate(percent = sentiment_year_count / year_total * 100 ) %>%
    #filter(year %in% c("1978","1994","1995")) %>%
    select(-sentiment_year_count, -year_total) %>%
    spread(created_at, percent) %>%
    chartJSRadar(showToolTipLabel = TRUE,
                 main = "NRC video Radar", colMatrix =col)
  print(year_radar_chart)





  col <- col2rgb(c("peachpuff", "royalblue", "tomato", "#B4CF68", "green", "purple", "orange","grey","red","#8DD3C7" , "pink", "blue","gold"))
  year_radar_chart <- year_sentiment_nrc %>%
    inner_join(total_sentiment_video, by = "created_at") %>%
    mutate(percent = sentiment_year_count / year_total * 100 ) %>%
    #filter(year %in% c("1978","1994","1995")) %>%
    select(-sentiment_year_count, -year_total) %>%
    spread(created_at, percent) %>%
    chartJSRadar(showToolTipLabel = TRUE,
                 main = "NRC video Radar", colMatrix =col)

  #Join the two and create a percent field
  year_radar_chart <- dd %>%
    mutate(created_at = as.Date(created_at, format = "%y"),
           prob = prob*100) %>%
    select(created_at, topic, prob) %>%
    spread(created_at,  prob) %>%
    chartJSRadar(showToolTipLabel = TRUE,
                 main = "NRC video Radar", colMatrix =col)
  print(year_radar_chart)
}

year_radar_chart <- dd %>%
  group_by(created_at, topic) %>%
  mutate(created_at = format(created_at, format = "%Y"),
         prob = prob,
         topic = paste("Topic", topic, sep = " ")) %>%
  select(created_at, topic, prob) %>%
  distinct()

circos.clear() #very important! Reset the circular layout parameters
#assign colors to the outside bars around the circle
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00", "#D65E00")
grid.col = c("2021" = my_colors[1],
             "2020" = my_colors[2],
             "2019" = my_colors[3],
             "Topic 1" = "grey", "Topic 2" = "grey", "Topic 3" = "grey", "Topic 4" = "grey","Topic 5" = "grey","Topic 6" = "grey","Topic 7" = "grey")

# set the global parameters for the circular layout. Specifically the gap size (15)
#this also determines that topic goes on top half and source on bottom half
circos.par(gap.after = c(rep(5, length(unique(year_radar_chart[[1]])) - 1), 15,
                         rep(5, length(unique(year_radar_chart[[2]])) - 1), 15))
circos.par(track.margin=c(0,0))
#main function that draws the diagram. transparancy goes from 0-1
chordDiagram(year_radar_chart, grid.col = grid.col, transparency = .2)

