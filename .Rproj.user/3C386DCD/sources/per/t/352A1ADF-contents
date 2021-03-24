tabPanel("Mallet words cloud",
         sidebarLayout(
           sidebarPanel(

             # Input: Checkbox if file has url
             numericInput("mt_wc", "Num words in cloud", 200, min = 1, max = 800),

             # Input: Checkbox if file has url
             #numericInput("n_t", "Choose topic number", 1, min = 1, max = 100),
             # Input: Checkbox if file has url
             numericInput("par_row", "screen layout row",value = 3, min = 1, max = 10),
             # Input: Checkbox if file has url
             numericInput("par_col", "screen layout col",value = 4, min = 1, max = 10),
             # Horizontal line
             tags$hr(),

             ##action button
             actionButton("malletcloud", "Mallet topic wordcloud", class = "btn-primary")
           ),


           # Main panel for displaying outputs
           mainPanel(

             # Output: Data file
             plotOutput("mallet_wordscloud",width = "100%", height = "600px")

           )
         )
)


plot_mallet <- eventReactive(input$malletcloud,{
  withProgress(message = 'Progress for plot..', value = 10, {
    req(topic_mallet_list())
    pal2 <- brewer.pal(8,"Dark2")
    #df_words <- as.data.frame(topic_mallet_list()[input$n_t])
    #wordcloud(df_words$words, df_words$weights, scale = c(5,.5), #min.freq = 100,
    #max.words = input$mt_wc, random.order = FALSE, rot.per = .15, colors = pal2, main = paste0("wordcloud topic", input$n_t))
    par(mfrow = c(input$par_row, input$par_col),     # 2x2 layout
        oma = c(2, 2, 0, 0), # two rows of text at the outer left and bottom margin
        mar = c(1, 1, 0, 0), # space for one row of text at ticks and to separate plots
        mgp = c(2, 1, 0),    # axis label at 2 rows distance, tick labels at 1 row
        xpd = NA)

    for(i in 1:length(topic_mallet_list())){
      df_words <- as.data.frame(topic_mallet_list()[i])
      wordcloud(df_words$words, df_words$weights, scale = c(3,.2), #min.freq = 100,
                max.words = input$mt_wc, random.order = FALSE, rot.per = .15, colors = pal2, main = "Mallet wordcloud")
    }
  })
})

output$mallet_wordscloud <- renderPlot({
  #req(topic_mallet_list())
  #pal2 <- brewer.pal(8,"Dark2")
  #par(mfrow=c(3,4))
  #for(i in 1:10){
  #   df_words <- as.data.frame(topic_mallet_list()[i])
  #  wordcloud(df_words$words, df_words$weights, scale = c(3,.2), #min.freq = 100,
  #         max.words = input$mt_wc, random.order = FALSE, rot.per = .15, colors = pal2)
  #}
  plot_mallet()
})
