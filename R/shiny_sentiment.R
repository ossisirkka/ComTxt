## Semantic network for anger and fear sadness---------------------

library(shinydashboard)
library(shiny)
library(shinyjqui)
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
library(quanteda)
library(quanteda.textplots)

shiny_sentiment <- function(sent_df){
  require(shiny)
  require(ggrepel)
  shinyApp(
    ui = navbarPage(
      sidebarLayout(
        sidebarPanel(
          selectInput("variable", "Variable:",
                      c("Year" = "%Y",
                        "Month" = "%m/%Y")),
          textInput("select", "Emotion Category", "Surprise"),
          sliderInput("n_nodes", "Number of words:",
                      min = 50, max = 500, value = 100)
          ),
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("sentiment words plot", jqui_resizable(plotOutput("sent_word"))),
                      tabPanel("sentiment bar plot", jqui_resizable(plotOutput("sent_bar"))),
                      tabPanel("sent radar map", jqui_resizable(chartJSRadarOutput("sent_radar"))),
                      tabPanel("sent network", jqui_resizable(plotOutput("sent_network")))
          )
        )
      )
    ),
    server = function(input,output) {

      df_tmp <- reactive({
        df %>%
        group_by(sentiment) %>%
        count(word, sort = TRUE) %>%
        arrange(desc(n)) %>%
        slice(seq_len(10)) %>%
        ungroup()
        })


      output$sent_word <- renderPlot(
       df_tmp() %>%
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
      )

      df_bar <- reactive({
        df_bar <- df
        df_bar$created_at <- format(df_bar$created_at, input$variable)
        return(df_bar)
      })

      output$sent_bar <- renderPlot(
        df_bar() %>%
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
      )

      df_radar <- reactive({
        df_radar <- df
        df_radar$created_at <- format(df_radar$created_at, input$variable)
        return(df_radar)
      })


      #prince_nrc_sub <- df %>%
      #filter(!sentiment %in% c("positive", "negative"))

      year_sentiment_nrc <- reactive({df_radar()  %>%
        group_by(created_at, sentiment) %>%
        count(created_at, sentiment) %>%
        select(created_at, sentiment, sentiment_year_count = n)
      })

      #Get the total count of sentiment words per video_name (not distinct)
      total_sentiment_video <- reactive({df_radar() %>%
        count(created_at) %>%
        select(created_at, year_total = n)
      })

      col <- reactive({col2rgb(c(brewer.pal(8, "Accent"), brewer.pal(8, "Dark2"), brewer.pal(12, "Paired")))})
      #Join the two and create a percent field
      year_radar_chart <- reactive({
        year_sentiment_nrc() %>%
        inner_join(total_sentiment_video(), by = "created_at") %>%
        mutate(percent = sentiment_year_count / year_total * 100 ) %>%
        #filter(year %in% c("1978","1994","1995")) %>%
        select(-sentiment_year_count, -year_total) %>%
        spread(created_at, percent) })

     output$topic_radar <- renderChartJSRadar(
       chartJSRadar(year_radar_chart(), showToolTipLabel = TRUE,
                    main = "Topic Radar", colMatrix =col())
     )


     topgat_fcm <- reactive({
       select_nrc <- df %>%
       filter(sentiment %in% input$select)
       user_data <- unique(select_nrc()$status_id)

     net <- list()
     for (i in 1:length(user_data)){
       if(length(select_nrc[select_nrc$status_id == user_data[i],]$word)>1){
         word <- paste(unique(select_nrc[select_nrc$status_id == user_data[i],]$word), sep = "", collapse = " ")
       } else {
         word<- select_nrc[select_nrc$status_id == user_data[i],]$word
       }
       net[[i]] <- as.data.frame(cbind(id = user_data[i], word))
     }
     net_anger <- do.call(rbind, net)
     tweets_dfm <- dfm(net_anger$word) # Document-feature matrix
     #tag_dfm <- dfm_select(tweets_dfm, pattern = ("#*")) # Get hashtags
     toptag <- names(topfeatures(tweets_dfm, input$n_nodes)) # Most important hashtags; we dont want to plot every hashtag
     tag_fcm <- fcm(tweets_dfm) # Feature-occurance matrix which shows the network structure
     fcm_select(tag_fcm, pattern = toptag) # Filter results so that we plot only 50 top hashtags
     })

     output$sent_network <- renderPlot(
     textplot_network(topgat_fcm(),  edge_alpha = 0.5, edge_size = 5, edge_color = "grey",vertex_color ="#538797")
     )

    }
  )
}





