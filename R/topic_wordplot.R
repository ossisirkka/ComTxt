##topic_wordsplot
library(dplyr)
library(tidytext)


#create function that accepts the lda model and num word to display
topic_wordplot <- function(mallet_df, k, num_words = 10) {
  topic.words <- mallet.topic.words(mallet_df, smoothed = T, normalized = T)
  mallet_words_list <- list()
  for (i in 1:as.numeric(k)) {
    mallet_words_list[[i]] <- mallet.top.words(mallet_df, topic.words[i,], 100)
  }
  topic_mallet_list <- mallet_words_list

  topic_word <- list.cbind(topic_mallet_list)

  ## top num_words words per topic
  sort <- list()
  for(i in 1:k*2-1){
    sort[[i]] <- c(i,i+1)
  }
  sort <- do.call(rbind,sort)

  tt <- list()
  for(i in 1:nrow(sort)){
    tt[[i]] <- cbind(topic = paste("Topic", i), topic_word[,c(sort[i,])])
  }

  topics_tidy <- do.call(rbind.data.frame, tt)

  theme_lyrics <- function(aticks = element_blank(),
                           pgminor = element_blank(),
                           lt = element_blank(),
                           lp = "none")
  {
    theme(plot.title = element_text(hjust = 0.5), #center the title
          axis.ticks = aticks, #set axis ticks to on or off
          panel.grid.minor = pgminor, #turn on or off the minor grid lines
          legend.title = lt, #turn on or off the legend title
          legend.position = lp) #turn on or off the legend
  }

  word_chart <- function(data, input, title) {
    data %>%
      #set y = 1 to just plot one variable and use word as the label
      ggplot(aes(as.factor(row), 1, label = input, fill = factor(topic) )) +
      #you want the words, not the points
      geom_point(color = "transparent") +
      #make sure the labels don't overlap
      geom_label_repel(nudge_x = .2,
                       direction = "y",
                       box.padding = 0.1,
                       segment.color = "transparent",
                       size = 3) +
      facet_grid(~topic) +
      theme_lyrics() +
      theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
            #axis.title.x = element_text(size = 9),
            panel.grid = element_blank(), panel.background = element_blank(),
            panel.border = element_rect("lightgray", fill = NA),
            strip.text.x = element_text(size = 9)) +
      labs(x = NULL, y = NULL, title = title) +
      #xlab(NULL) + ylab(NULL) +
      #ggtitle(title) +
      coord_flip()
  }

top_terms <- topics_tidy %>%
    group_by(topic) %>%
    arrange(topic, desc(weights)) %>%
    #get the top num_words PER topic
    slice(seq_len(num_words)) %>%
    arrange(topic, weights) %>%
    #row is required for the word_chart() function
    mutate(row = row_number()) %>%
    ungroup()
  #create a title to pass to word_chart
  title <- paste("Mallet Top",num_words  ,"Terms for", k, "Topics")
  #call the word_chart function you built in prep work
  word_chart(top_terms, top_terms$words, title)
}
