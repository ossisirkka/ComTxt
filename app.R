library(shinydashboard)
library(shiny)
## preprocessing
library(udpipe)
library(dplyr)
## topicmodelling
library(mallet)
library(stopwords)
##visualizing topic
library(RColorBrewer)
library(wordcloud)
library(rlist)
library(kableExtra)
library(igraph)
##semantic network
library(quanteda)
##sentiment analysis
library(tidytext) #Text mining
library(tidyr) #Spread, separate, unite, text mining (also included in the tidyverse package)
library(widyr) #Use for pairwise correlation
library(textdata)
library(readxl)
##visualizing sentiment
#Visualizations!
library(ggplot2) #Visualizations (also included in the tidyverse package)
library(ggrepel) #`geom_label_repel`
library(gridExtra) #`grid.arrange()` for multi-graphs
library(knitr) #Create nicely formatted output tables
library(formattable) #For the color_tile function
library(circlize) #Visualizations - chord diagram
library(memery) #Memes - images with plots
library(magick) #Memes - images with plots (image_read)
library(yarrr)  #Pirate plot
library(radarchart) #Visualizations
library(igraph) #ngram network diagrams
library(ggraph)
## call load packages
library(SparseM)
library(lava)
library(RTextTools)
library(tm)
library(slam)
library(topicmodels)
library(Rmpfr)
## make a plot with
library(ggplot2)

library(grid)
library(gridExtra)


# Define UI for application that draws a histogram
ui <-
    navbarPage("Twitter", ##add tab
              ##Panel 1: Data Scraping
               #navbarMenu("Data Scraping",
                          ##search trend scraping-----
                          tabPanel("trend",
                            sidebarLayout(
                              sidebarPanel(
                              textInput("trend_to_search",
                                        "Search Trend location:",
                                        value = "barcelona"
                              ),
                              actionButton("get_trend", "Get trend", class = "btn-primary")
                            ),
                            mainPanel(
                              tabsetPanel(type = "tabs",
                                tabPanel("table", reactableOutput("trend_table")),
                                tabPanel("Trend graph", plotOutput("trendsearch_graph", width = "100%", height = "1000px")))
                            )
                            )
                          ),
                          ##search tweet scraping----------
                          tabPanel("tweets",
                                   sidebarLayout(## Sidebar with a slider input for number of bins
                                       sidebarPanel(
                                           sliderInput("num_tweets_to_download",
                                                       "Number of tweets to download:",
                                                       min = 50,
                                                       max = 300,
                                                       value = 50,
                                                       step = 10
                                           ),
                                           textInput("hashtags_to_search",
                                                     "hashtag to search:",
                                                     value = "#business"
                                           ),
                                           textInput("hashtags_to_search_2",
                                                     "hashtag to search:",
                                                     value = "#University"
                                           ),

                                           textInput("language_to_search",
                                                     "language to search:",
                                                     value = "es"
                                           ),
                                           textInput("location_to_search",
                                                     "location to search:",
                                                     value = "barcelona"
                                           ),
                                           actionButton("get_hashtag", "Get data", class = "btn-primary"),
                                           ## Horizontal line
                                           tags$hr(),

                                           #textInput("savefile",
                                          #           "file name:",
                                           #          value = "Msc"
                                           #),
                                           # Button
                                           #downloadButton("searchTweet", "Download"),

                                           ## Horizontal line
                                           tags$hr(),

                                           ##graph selection
                                           selectInput("ts_query", "post by:",
                                                      c("day" = "days","hour" = "hours")
                                          )

                                       ),

                                       mainPanel(
                                         tabsetPanel(type = "tabs",
                                           tabPanel("table", reactableOutput("query_tweet_table")),
                                           tabPanel("Trend graph", plotOutput("query_graph", width = "100%")),
                                           tabPanel("Wordscloud", plotOutput("wordcloud_plot")),
                                           tabPanel("Hashtag chart", plotOutput("hash_bar"), height = "150%"),
                                           tabPanel("Hashtag trend", plotOutput("hash_trend"), height = "150%"),
                                           tabPanel("Hashtag network", plotOutput("hash_network"), height = "150%"),
                                           tabPanel("mention network", plotOutput("network_graph"), height = "150%"),
                                           tabPanel("Location Map", plotOutput("map_plot"), height = "150%"))

                                       )
                                   )
                          ),## close the tabPanel for tweets
                          ##timeline scraping----
                          tabPanel("timeline",
                                   ## SidebarLayout 1
                                   sidebarLayout(
                                     ##siderbarPanel 1
                                     sidebarPanel(
                                           ##sliderInput number of timeline
                                           sliderInput("num_timeline_to_download",
                                                       "Number of timeline to download:",
                                                       min = 100,
                                                       max = 500,
                                                       value = 100,
                                                       step = 100
                                           ),
                                           #textInput userID
                                           textInput("userid_to_search_1",
                                                     "UserID to search:",
                                                     value = "amazon"
                                           ),
                                           #textInput userID
                                           textInput("userid_to_search_2",
                                                   "UserID to search:",
                                                   value = "amazonhelp"
                                            ),
                                          #textInput userID
                                           textInput("userid_to_search_3",
                                                     "UserID to search:",
                                                    value = "amazonnews"
                                            ),
                                           ##actionButton
                                           actionButton("get_data", "Get data", class = "btn-primary"),
                                       ## Horizontal line
                                       tags$hr(),

                                       ##graph selection
                                       selectInput("ts_timeline", "post by:",
                                         c("day" = "days","hour" = "hours"
                                           )
                                       )),

                                           ##mainPanler 1
                                           mainPanel(
                                             tabsetPanel(type = "tabs",
                                               tabPanel("table", reactableOutput("tweet_table")),
                                               tabPanel("Trend graph", plotOutput("timeline_graph", width = "100%")))
                                             #reactableOutput("tweet_table")
                                           )
                                     ##close Panler 1
                                   )
                                   ## close sidebarLayout 1
                                   )
    )


options(shiny.maxRequestSize = 100*1024^2)




##server --------------------------------------
# Define server logic required to draw a histogram
server <- function(input, output) {

    ##twitter oauth

    ## the name you assigned to your created app
    appname <- "ComTexAnalysis"

    ##api key
    key <- "qGoBFjggRplKb7D14eWbwGlQq"

    ## api secret
    secret <- "DTEjQWbLPOYMxPBUFJDF9sERAH9TCDR91xAWOXwTo2o5ucs03i"

    ##access_token
    access_token <- "1229767227446087680-AUkLO6rrAlDEpyxCjh8p44K2gf9WTH"

    ##access_secret
    access_secret <- "rR6nKBuMLl8xsIqY6oQ9nHuxe2HEtW2S9Q9nW6BDVob2h"

    ## create token named "twitter_token"
    twitter_token <- create_token(
        app = appname,
        consumer_key = key,
        consumer_secret = secret,
        access_token = access_token,
        access_secret = access_secret)

    get_token()

## Panel 0 get trend ----
    trend_df <- eventReactive(input$get_trend,{
      get_trends(input$trend_to_search)
    })

    trend_table_data <- reactive({
      req(trend_df())
      trend_df() %>%
        select(trend, tweet_volume, place, created_at, url) %>%
        mutate(
          URL = purrr::map_chr(url, make_url_html)
        ) %>%
        select(Trend = trend, Volumne = tweet_volume, Place = place, Created_at = created_at,URL)
    })

    output$trend_table <- renderReactable({
      reactable::reactable(trend_table_data(),
        filterable = TRUE,
        searchable = TRUE,
        bordered = TRUE,
        striped = TRUE,
        highlight = TRUE,
        showSortable = TRUE,
        defaultSortOrder = "desc",
        defaultPageSize = 25,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(25, 50, 75, 100, 200),
        columns = list(
          Trend = colDef(defaultSortOrder = "asc"),
          Volumne = colDef(filterable = FALSE, format = colFormat(separators = TRUE)),
          Place = colDef(defaultSortOrder = "asc"),
          Created_at = colDef(defaultSortOrder = "asc"),
          URL = colDef(html = TRUE)
        )
      )
    })

    trend_df_1 <- reactive({
      req(trend_df())
      df <- trend_df()[!is.na(trend_df()$tweet_volume),]
      df <- df %>% arrange(tweet_volume)
      df$trend <- factor(df$trend,levels = as.character(df$trend))
      as.data.frame(df)
    })

    output$trendsearch_graph <- renderPlot({
      trend_df_1() %>%
        ggplot2::ggplot(aes(x=tweet_volume, y=trend, fill = trend))+
        ggplot2::geom_bar(stat="identity") +
        ggplot2::geom_text(aes(label=tweet_volume), vjust=1.6, color="black", size=3.5)+
        ggplot2::theme_classic()+
        ggplot2::theme(
          legend.position  = "none",
          plot.title = ggplot2::element_text(face = "bold")) +
        ggplot2::labs(
          y = NULL,
          title = "Trend in Twitter",
          subtitle = paste("trend in", input$trend_to_search),
          caption = "\nSource: Data collected from Twitter's REST API via rtweet"
        )
    })

## panel 1.1 hashtag download------
    rtweet_df <- eventReactive(input$get_hashtag,{
      search_tweets(paste(input$hashtags_to_search, "OR", input$hashtags_to_search_2), n = input$num_tweets_to_download, lang = input$language_to_search, geocode = lookup_coords(as.character(input$location_to_search)),include_rts = FALSE, retryonratelimit = TRUE)
    })

    rtweet_table_data <- reactive({
      req(rtweet_df())
      rtweet_df() %>%
        select(user_id, status_id, created_at, screen_name, text, favorite_count, retweet_count, hashtags, urls_expanded_url) %>%
        mutate(
          Tweet = glue::glue("{text} <a href='https://twitter.com/{screen_name}/status/{status_id}'>>> </a>"),
          URLs = purrr::map_chr(urls_expanded_url, make_url_html)
        ) %>%
        select(DateTime = created_at, User = screen_name, Tweet = text, Likes = favorite_count, RTs = retweet_count, Hashtags = hashtags, URLs)
    })

    output$query_tweet_table <- renderReactable({
      reactable::reactable(rtweet_table_data(),
                           filterable = TRUE,
                           searchable = TRUE,
                           bordered = TRUE,
                           striped = TRUE,
                           highlight = TRUE,
                           showSortable = TRUE,
                           defaultSortOrder = "desc",
                           defaultPageSize = 25,
                           showPageSizeOptions = TRUE,
                           pageSizeOptions = c(25, 50, 75, 100, 200),
                           columns = list(
                             DateTime = colDef(defaultSortOrder = "asc"),
                             User = colDef(defaultSortOrder = "asc"),
                             Tweet = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
                             Likes = colDef(filterable = FALSE, format = colFormat(separators = TRUE)),
                             RTs = colDef(filterable =  FALSE, format = colFormat(separators = TRUE)),
                             Hashtags = colDef(defaultSortOrder = "asc"),
                             URLs = colDef(html = TRUE)
                           )
      )
    })

    output$query_graph <- renderPlot({
      rtweet_df() %>%
        #dplyr::group_by(screen_name) %>%
        ts_plot(input$ts_query, trim = 1L) +
        ggplot2::geom_point() +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          legend.title = ggplot2::element_blank(),
          legend.position = "bottom",
          plot.title = ggplot2::element_text(face = "bold")) +
        ggplot2::labs(
          x = NULL, y = NULL,
          title = "Frequency of Twitter statuses posted",
          subtitle = paste("Twitter status (tweet) counts aggregated by", input$ts_query),
          caption = "\nSource: Data collected from Twitter's REST API via rtweet"
        )
    })
    wordcloud_rep <- repeatable(wordcloud_plot)

    output$wordcloud_plot <- renderPlot({
      req(rtweet_df())
      tmp <- tokens(char_tolower(rtweet_df()$text), remove_punct = TRUE, remove_url = TRUE, remove_numbers = TRUE)
      tmp <- tokens_remove(tmp, stopwords(input$language_to_search), padding = TRUE)
      tmp <- tokens_remove(tmp, c("#", "@"), valuetype = "regex", padding = TRUE)
      tmp<- tokens_wordstem(tmp, language = input$language_to_search)
      tmp<-dfm(tmp)
      tmp <-dfm_select(tmp, min_nchar = 3)
      tmp <-dfm(tmp, remove = c("del", "com", "sua"))
      textplot_wordcloud(tmp, rotation = 0.25,
                         color = RColorBrewer::brewer.pal(8, "Dark2"), max_words = 500, min_count = 1)
    })

    output$hash_bar <- renderPlot({
      req(rtweet_df())
      all_hashtags <- tolower(unlist(rtweet_df()$hashtags))
      hashtag_frequencies <- sort(table(all_hashtags), decreasing = TRUE)
      hashtag_frequencies <- data.frame(hashtag_frequencies)
      hashtag_frequencies <-head(hashtag_frequencies,10)

      hashtag_frequencies %>%
        ggplot2::ggplot(aes(x=Freq, y=all_hashtags, fill = all_hashtags))+
        ggplot2::geom_bar(stat="identity") +
        ggplot2::geom_text(aes(label=Freq), vjust=1.6, color="black", size=3.5)+
        ggplot2::theme_classic()+
        ggplot2::theme(
          legend.position  = "none",
          plot.title = ggplot2::element_text(face = "bold")) +
        ggplot2::labs(
          y = NULL,
          title = "Hashtags trend",
          subtitle = paste("trend in", "hashtgas"),
          caption = "\nSource: Data collected from Twitter's REST API via rtweet"
        )
    })

    output$hash_trend <- renderPlot({
      req(rtweet_df())
      hashtag_df <- select(rtweet_df(), created_at, hashtags)

      tmp <- list()
      for(i in 1:nrow(hashtag_df)){
        tmp.hashtags <- hashtag_df$hashtags[i][[1]]
        tmp.created_at <- rep(hashtag_df$created_at[i], length(tmp.hashtags))
        tmp[[i]] <- data.frame(created_at = tmp.created_at, hashtags = tmp.hashtags)
      }

      tmp.1 <- as.data.frame(do.call("rbind", tmp))
      tmp.1$hashtags <- as.character(tmp.1$hashtags)
      tmp.1$hashtags <- stri_trans_general(str = tmp.1$hashtags,
                                           id = "Latin-ASCII")
      tmp.1$hashtags <- tolower(tmp.1$hashtags)
      tmp.3 <- na.omit(tmp.1)
      tmp.3$hashtags <- gsub("[0-9]|[0-9]", "", tmp.3$hashtags)
      tmp.3$hashtags <- gsub("<u...*>", NA, tmp.3$hashtags)
      tmp.3$hashtags <- gsub("[[:punct:][:blank:]]", "", tmp.3$hashtags)

      tmp.3 <- na.omit(tmp.3)

      tmp.3$hashtags <- paste0("#",tmp.3$hashtags)


      tm <- as.data.frame(tmp.3$hashtags)
      tm <- data.frame(table(tm))
      tm <- arrange(tm,desc(Freq))
      tm <- tm[1:10,]

      tmp.3 <- tmp.3[tmp.3$hashtags %in% as.character(tm$tm),]
      tmp.3 <-  group_by(tmp.3,hashtags)

      ts_plot(tmp.3, as.character("3 hours"), trim = 2L, size = 1) +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
        ggplot2::labs(
          x = NULL, y = NULL,
          title = "Frequency of # Twitter statuses Top 10",
          subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
          caption = "\nSource: Data collected from Twitter's REST API via rtweet"
        )

    })

    output$hash_network <- renderPlot({
      req(rtweet_df())
      tweets_dfm <- dfm(rtweet_df()$text) # Document-feature matrix
      tag_dfm <- dfm_select(tweets_dfm, pattern = ("#*")) # Get hashtags
      toptag <- names(topfeatures(tag_dfm, 100)) # Most important hashtags; we dont want to plot every hashtag
      tag_fcm <- fcm(tag_dfm) # Feature-occurance matrix which shows the network structure
      topgat_fcm <- fcm_select(tag_fcm, pattern = toptag) # Filter results so that we plot only 50 top hashtags
      textplot_network(topgat_fcm, min_freq = 0.1, edge_alpha = 0.5, edge_size = 5, edge_color = "grey",vertex_color ="#538797")
    })

    output$network_graph <-renderPlot({
      isolate({
        withProgress({
          setProgress(message = "Processing network...")

      req(rtweet_df())
      tweets_dfm <- dfm(rtweet_df()$text)
      tag_dfm <- dfm_select(tweets_dfm, pattern = ("@*")) # Get hashtags
      toptag <- names(topfeatures(tag_dfm, 50)) # Most important hashtags; we dont want to plot every hashtag
      tag_fcm <- fcm(tag_dfm) # Feature-occurance matrix which shows the network structure
      df_edges <- fcm_select(tag_fcm, pattern = toptag)
      textplot_network(df_edges, min_freq = 0.1, edge_alpha = 0.5, edge_size = 5, edge_color = "grey",vertex_color ="#538797")
    })
   })
  })
    output$map_plot <- renderPlot({
      withProgress(message = 'drawing map', value = 10, {
      req(rtweet_df())
      df_loca <- lat_lng(rtweet_df())
      par(mar = c(0, 0, 0, 0))
      maps::map('world', 'spain')
      title("tweets in Spain")
      ## plot lat and lng points onto state map
      with(df_loca, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))
      })
    })

    df_save <- reactive({
      req(rtweet_df())
      df <- rtweet_df()
      df$hashtags <- sapply(df$hashtags,paste, collapse = "," )## change hastags list into characters

      df$symbols <- sapply(df$symbols, paste, collapse = "," ) ## change symbols list into characters

      df$urls_url <- sapply(df$urls_url, paste, collapse = "," ) ## change url list into characters

      df$urls_t.co <- sapply(df$urls_t.co, paste, collapse = "," ) ## change urls_t.co  list into characters

      df$urls_expanded_url <- sapply(df$urls_expanded_url, paste, collapse = "," ) ## change $urls_expanded_url  list into characters

      df$media_url <- sapply(df$media_url, paste, collapse = "," ) ## change $media_url  list into characters

      df$media_t.co <- sapply(df$media_t.co, paste, collapse = "," ) ## change $media_t.co list into characters

      df$media_expanded_url <- sapply(df$media_expanded_url, paste, collapse = "," ) ## change $media_expanded_url list into characters

      df$media_type <- sapply(df$media_type, paste, collapse = "," ) ## change $media_type list into characters

      df$ext_media_url <- sapply(df$ext_media_url, paste, collapse = "," ) ## change $ext_media_url list into characters

      df$ext_media_t.co <- sapply(df$ext_media_t.co, paste, collapse = "," ) ## change $ext_media_t.co list into characters

      df$ext_media_expanded_url <- sapply(df$ext_media_expanded_url, paste, collapse = "," ) ## change $ext_media_expanded_url list into characters

      df$mentions_user_id <- sapply(df$mentions_user_id, paste, collapse = "," ) ## change $mentions_user_id list into characters

      df$mentions_screen_name <- sapply(df$mentions_screen_name, paste, collapse = "," ) ## change $mentions_screen_name list into characters

      df$geo_coords <- sapply(df$geo_coords, paste, collapse = "," ) ## change $geo_coords list into characters

      df$coords_coords <- sapply(df$coords_coords, paste, collapse = "," ) ## change $coords_coords list into characters

      df$bbox_coords <- sapply(df$bbox_coords, paste, collapse = "," ) ## change $bbox_coords list into characters
      df
    })

    # Downloadable csv of selected dataset
    output$searchTweet <- downloadHandler(
      filename = function() {
        paste(input$savefile, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(df_save(), file, row.names = FALSE)
      }
    )

##Panel 1.2: timeline download -------
    tweet_df <- eventReactive(input$get_data,{
        get_timeline(c(input$userid_to_search_1,input$userid_to_search_2,input$userid_to_search_3 ), n = input$num_timeline_to_download, include_rts = FALSE, retryonratelimit = TRUE)
    })

    tweet_table_data <- reactive({
        req(tweet_df())
        tweet_df() %>%
            select(user_id, status_id, created_at, screen_name, text, favorite_count, retweet_count, hashtags, urls_expanded_url) %>%
            mutate(
                Tweet = glue::glue("{text} <a href='https://twitter.com/{screen_name}/status/{status_id}'>>> </a>"),
                URLs = purrr::map_chr(urls_expanded_url, make_url_html)
            ) %>%
            select(DateTime = created_at, User = screen_name, Tweet = text, Likes = favorite_count, RTs = retweet_count, Hashtags = hashtags, URLs)


    })

    output$tweet_table <- renderReactable({
        reactable::reactable(tweet_table_data(),
                             filterable = TRUE,
                             searchable = TRUE,
                             bordered = TRUE,
                             striped = TRUE,
                             highlight = TRUE,
                             showSortable = TRUE,
                             defaultSortOrder = "desc",
                             defaultPageSize = 25,
                             showPageSizeOptions = TRUE,
                             pageSizeOptions = c(25, 50, 75, 100, 200),
                             columns = list(
                                 DateTime = colDef(defaultSortOrder = "asc"),
                                 User = colDef(defaultSortOrder = "asc"),
                                 Tweet = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
                                 Likes = colDef(filterable = FALSE, format = colFormat(separators = TRUE)),
                                 RTs = colDef(filterable =  FALSE, format = colFormat(separators = TRUE)),
                                 Hashtags = colDef(defaultSortOrder = "asc"),
                                 URLs = colDef(html = TRUE)
                             )
        )
    })

    output$timeline_graph <- renderPlot({
      tweet_df() %>%
        dplyr::group_by(screen_name) %>%
        ts_plot(input$ts_timeline, trim = 1L) +
        ggplot2::geom_point() +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          legend.title = ggplot2::element_blank(),
          legend.position = "bottom",
          plot.title = ggplot2::element_text(face = "bold")) +
        ggplot2::labs(
          x = NULL, y = NULL,
          title = "Frequency of Twitter statuses posted",
          subtitle = paste("Twitter status (tweet) counts aggregated by", input$ts_timeline),
          caption = "\nSource: Data collected from Twitter's REST API via rtweet"
        )
      })


## panel 7 network igraph----------
    ## making igraph data
    #df_edges  <- eventReactive(input$get_igraph,{
    #isolate({
        # withProgress({
    # setProgress(message = "Processing network...")

    #tweets_dfm <- dfm(rtweet_df()$text)
    #tag_dfm <- dfm_select(tweets_dfm, pattern = ("@*")) # Get hashtags
    #toptag <- names(topfeatures(tag_dfm, input$num_communication)) # Most important hashtags; we dont want to plot every hashtag
    #tag_fcm <- fcm(tag_dfm) # Feature-occurance matrix which shows the network structure
    #fcm_select(tag_fcm, pattern = toptag) # Filter results so that we plot only 50 top hashtags
    #})
    #})
    #})

    ##plot
    #output$network_graph <-  renderPlot({
    #textplot_network(df_edges(), min_freq = 0.1, edge_alpha = 0.5, edge_size = 5, edge_color = "grey",vertex_color ="#538797")
    #})

}

# Run the application
shinyApp(ui = ui, server = server)
