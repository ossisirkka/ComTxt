## Semantic network for anger and fear sadness---------------------

library(shinydashboard)
library(shiny)
library(reactable)
library(dplyr)
library(tidytext)
library(mallet)
library(rlist)
library(ggrepel)
library(mallet)
library(stopwords)
library(scales)
library(tidyr)
library(radarchart)
library(shinyjqui)

shiny_topic <- function(mallet_df, df){
  require(shiny)
  require(ggrepel)

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
  shinyApp(
    ui = navbarPage(
      sidebarLayout(
        sidebarPanel(
          #numericInput("n_topic", "Number K", 7, min = 2, max = 100),
          numericInput("num_words", "Number Words", 10, min = 2, max = 50)),
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("Topic words plot", jqui_resizable(plotOutput("topic_word"))),
                      tabPanel("Topic radar map", jqui_resizable(chartJSRadarOutput("topic_radar")))
          )
        )
      )
    ),
    server = function(input,output) {
      top_terms <- reactive({
        topic.words <- mallet.topic.words(mallet_df, smoothed = T, normalized = T)
        mallet_words_list <- list()
        for (i in 1:as.numeric(mallet_df$model$numTopics)) {
          mallet_words_list[[i]] <- mallet.top.words(mallet_df, topic.words[i,], 100)
        }
        topic_mallet_list <- mallet_words_list

        topic_word <- list.cbind(topic_mallet_list)

        ## top num_words words per topic
        sort <- list()
        for(i in 1:mallet_df$model$numTopics*2-1){
          sort[[i]] <- c(i,i+1)
        }
        sort <- do.call(rbind,sort)

        tt <- list()
        for(i in 1:nrow(sort)){
          tt[[i]] <- cbind(topic = paste("Topic", i), topic_word[,c(sort[i,])])
        }

        topics_tidy <- do.call(rbind.data.frame, tt)


        top_terms <- topics_tidy %>%
          group_by(topic) %>%
          arrange(topic, desc(weights)) %>%
          #get the top num_words PER topic
          slice(seq_len(input$num_words)) %>%
          arrange(topic, weights) %>%
          #row is required for the word_chart() function
          mutate(row = row_number()) %>%
          ungroup()
        return(top_terms)
      })

      title <- reactive({
        paste("Mallet Top",input$num_words  ,"Terms for",mallet_df$model$numTopics, "Topics")
      })

      output$topic_word <- renderPlot(
        #call the word_chart function you built in prep work
        word_chart(top_terms(), top_terms()$words, title())
      )

     year_topic <- reactive({
       doc.topics.m <- mallet.doc.topics(mallet_df, smoothed=T,
                                        normalized=T)


      tmp_df <- df[rep(seq_len(nrow(df)), each = mallet_df$model$numTopics), ]
      #tmp_df$topic <- paste("Topic", rep(1:n_topic,  nrow(df)))

      tt <- list()
      for(i in 1:nrow(doc.topics.m)){
        tt[[i]] <- doc.topics.m[i,]
      }

      tmp_df$prob <- unlist(tt)
      tmp_df$topic <- paste("Topic", rep(1:mallet_df$model$numTopics,  nrow(df)))
      tmp_df$created_at <- format(as.Date(tmp_df$created_at), "%Y")

      year_prob <- aggregate(x = tmp_df$prob,                # Specify data column
                             by = list(tmp_df$topic, tmp_df$created_at),              # Specify group indicator
                             FUN = sum)

      year_topic <- tmp_df  %>%
        group_by(created_at, topic) %>%
        count(created_at, topic) %>%
        select(created_at, topic, year_topic_n = n)

      year_topic$prob <- year_prob$x

      year_topic$percent <- year_topic$prob / year_topic$year_topic_n *100
      return(year_topic)
      })


     col <- reactive({
       col2rgb(c("peachpuff", "royalblue", "tomato", "#B4CF68", "green", "purple", "orange","grey","red","#8DD3C7" , "pink", "blue","gold"))
     })
       #Join the two and create a percent field
       year_radar_chart <- reactive({

         year_radar_chart <- year_topic() %>%
         select(-year_topic_n, -prob) %>%
         spread(created_at, percent)
         return(year_radar_chart)

       })

     output$topic_radar <- renderChartJSRadar(
       chartJSRadar(year_radar_chart(), showToolTipLabel = TRUE,
                    main = "Topic Radar", colMatrix =col())
     )

    }
  )
}





