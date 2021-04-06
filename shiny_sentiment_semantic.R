## Semantic network for anger and fear sadness---------------------
library(quanteda)
library(quanteda.textplots)
library(shinydashboard)
library(shiny)

shiny_sentiment_semantic <-  function(df) {
  require(shiny)
  shinyApp(
    ui = fluidPage(
      sidebarLayout(
        sidebarPanel(selectInput("sentiment_select", "Sentiment category:",
                                 c("Anger" = "Anger","Anticipation" = "Anticipation","Disgust" = "Disgust","Fear" = "Fear","Joy" = "Joy","Sadness" = "Sadness","Surprise" = "Surprise","Trust" = "Trust")),
                     #numericInput("top_n", "top token", 40, max = 200, min = 10 )
                     actionButton("semantic", "Semantic network Analysis", class = "btn-primary")),
        mainPanel(plotOutput("setsemanPlot"))
      )
    ),
    server = function(input, output) {
      fcm_local <- reactive({
        toks <- tokens(df$text, remove_punct = TRUE, remove_symbols = TRUE, verbose = TRUE)
        toks <- tokens_tolower(toks)
        toks <- tokens_remove(toks, padding = FALSE, min_nchar =3)

        ## A feature co-occurrence matrix
        fcmat <-quanteda::fcm(toks, context = "window", tri = FALSE)

        ##selecting keywords inside fcmat
        tmp <- fcmat[, as.character(input$sentiment_select)]
        tmp <- convert(tmp, to = "data.frame")
        colnames(tmp) <- c("nodes", "degree")
        tmp <- tmp[tmp$degree > 0, ]
        fcm_select(fcmat, pattern = c(tmp, as.character(input$sentiment_select)))
      })

      output$setsemanPlot <- renderPlot(
        textplot_network(fcm_local())
      )
    }
  )
}


binner <- function(var) {
  require(shiny)
  shinyApp(
    ui = fluidPage(
      sidebarLayout(
        sidebarPanel(sliderInput("n", "Bins", 5, 100, 20)),
        mainPanel(plotOutput("hist"))
      )
    ),
    server = function(input, output) {
      output$hist <- renderPlot(
        hist(var, breaks = input$n,
             col = "skyblue", border = "white")
      )
    }
  )
}

shiny_sentiment_semantic <- function(df_pre){

  ui <- fluidPage(

    # Application title
    titlePanel("Sentiment semantic network"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        selectInput("sentiment_select", "Sentiment category:",
                  c("Anger" = "Anger","Anticipation" = "Anticipation","Disgust" = "Disgust","Fear" = "Fear","Joy" = "Joy","Sadness" = "Sadness","Surprise" = "Surprise","Trust" = "Trust")),
        #numericInput("top_n", "top token", 40, max = 200, min = 10 )
        actionButton("semantic", "Semantic network Analysis", class = "btn-primary")
      ),

      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("setsemanPlot")
      )
    )
  )

  # Define server logic required to draw a histogram
  server <- function(input, output) {

    fcm_local <- eventReactive(input$semantic,{
    req(df_pre)
    toks <- tokens(df_pre$text, remove_punct = TRUE, remove_symbols = TRUE, verbose = TRUE)
    toks <- tokens_tolower(toks)
    toks <- tokens_remove(toks, padding = FALSE, min_nchar =3)


    ## A feature co-occurrence matrix
    fcmat <-quanteda::fcm(toks, context = "window", tri = FALSE)

    ##selecting keywords inside fcmat
    tmp <- fcmat[, input$sentiment_select]
    tmp <- convert(tmp, to = "data.frame")
    colnames(tmp) <- c("nodes", "degree")
    tmp <- tmp[tmp$degree > 0, ]

    fcm_select(fcmat, pattern = c(tmp, input$sentiment_select))
    })


    output$setsemanPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      textplot_network(fcm_local(),  edge_alpha = 0.5, edge_size = 5, edge_color = "grey",vertex_color ="#538797")
    })
  }

  # Run the application
  shinyApp(ui = ui, server = server)


}






