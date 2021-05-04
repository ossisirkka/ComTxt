## Semantic network for anger and fear sadness---------------------
library(quanteda)
library(quanteda.textplots)
library(shinydashboard)
library(shiny)
library(reactable)
library(shinyjqui)

shiny_mention <- function(df){
  require(shiny)
  require(reactable)
  shinyApp(
    ui = navbarPage(
      sidebarLayout(
        sidebarPanel(
        sliderInput("top_n", "Numbers of users", 5, 200, 20)),
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("user table", reactableOutput("user_table", width = "1000px", height = "auto")),
                      tabPanel("mention table", reactableOutput("mention_table")),
                      tabPanel("mention Network", jqui_resizable(plotOutput("mention_network")))
          )
        )
      )
    ),
    server = function(input,output) {

      df_user <- reactive({

      tmp <-df %>% select(user_id, screen_name, verified ,followers_count, friends_count, statuses_count)

      tmp <- tmp %>% distinct(user_id, .keep_all= TRUE)

      tmp$verified <- gsub(FALSE, "NO", tmp$verified)
      tmp$verified <- gsub(TRUE, "YES", tmp$verified)

      tmp_1 <- tmp %>%
        select(screen_name, verified ,followers_count) %>%
        arrange(desc(followers_count)) %>%
        head(input$top_n)

      tmp_2 <- tmp %>%
        select(screen_name, verified ,friends_count) %>%
        arrange(desc(friends_count)) %>%
        head(input$top_n)

      tmp_3 <- tmp %>%
        select(screen_name, verified ,statuses_count) %>%
        arrange(desc(statuses_count)) %>%
        head(input$top_n)

      cbind(tmp_1, tmp_2, tmp_3)
      })

      output$user_table <- renderReactable({
        reactable::reactable(df_user(),
                             filterable = TRUE,
                             searchable = TRUE,
                             bordered = TRUE,
                             striped = TRUE,
                             highlight = TRUE,
                             showSortable = TRUE,
                             defaultSortOrder = "desc",
                             defaultPageSize = 25,
                             showPageSizeOptions = TRUE
        )
      })

      toptag <- reactive({
        dd <- lapply(df$mentions_screen_name, data.frame)
        tmp <- list()
      for(i in 1:length(dd)){
        tmp[[i]] <- data.frame(from = df$screen_name[[i]], to = dd[[i]])
      }

        tmp <- do.call(rbind.data.frame, tmp)
        colnames(tmp) <- c("from", "to")
        df.network <- tmp
        df.network <-df.network[complete.cases(df.network), ]

        df.network$from <- paste0("@", df.network$from)
        df.network$to <- paste0("@", df.network$to)
        df.network$communication <- paste(df.network$from, df.network$to)

        hash_dfm <- dfm(df.network$communication)

        as.matrix(topfeatures(hash_dfm, input$top_n))
      })


      output$mention_table <- renderReactable({
        reactable::reactable(toptag(),
                             filterable = TRUE,
                             searchable = TRUE,
                             bordered = TRUE,
                             striped = TRUE,
                             highlight = TRUE,
                             showSortable = TRUE,
                             defaultSortOrder = "desc",
                             defaultPageSize = 25,
                             showPageSizeOptions = TRUE
        )
      })

      topgat_fcm  <- reactive({
        dd <- lapply(df$mentions_screen_name, data.frame)
        tmp <- list()
        for(i in 1:length(dd)){
          tmp[[i]] <- data.frame(from = df$screen_name[[i]], to = dd[[i]])
        }

        tmp <- do.call(rbind.data.frame, tmp)
        colnames(tmp) <- c("from", "to")
        df.network <- tmp
        df.network <-df.network[complete.cases(df.network), ]

        df.network$from <- paste0("@", df.network$from)
        df.network$to <- paste0("@", df.network$to)
        df.network$communication <- paste(df.network$from, df.network$to)


        hash_dfm <- dfm(df.network$communication)

        toptag <- names(topfeatures(hash_dfm, input$top_n)) # Most important mentions_screen_name; we dont want to plot every hashtag
        tag_fcm <- fcm(hash_dfm) # Feature-occurance matrix which shows the network structure
        fcm_select(tag_fcm, pattern = toptag) # Filter results so that we plot only 50 top mentions_screen_name
      })

      output$mention_network <- renderPlot(
        quanteda.textplots::textplot_network(topgat_fcm(), min_freq = 1, edge_color = "grey",vertex_color ="#538797"), height = 800, width = 1000 )

    }
  )
}





