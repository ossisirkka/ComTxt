tabPanel("Mallet semantic network",
         sidebarLayout(
           sidebarPanel(

             # Input: Checkbox if file has url
             numericInput("degree_weight", "Weight Words in Topic", 0.005, min = 0.0002, max = 0.008),

             # Input: Checkbox if file has url
             numericInput("igraph_semantic_degree", "V(net) degree",value = 80, min = 10, max = 100),
             # Input: Checkbox if file has url
             numericInput("igraph_semantic_vertex", "Vertext Size",value = 0.001, min = 0.0001, max = 0.006),
             # Input: Checkbox if file has url
             numericInput("igraph_semantic_label", "Vertext label size",value = 0.006, min = 0.0001, max = 0.01),
             # Horizontal line
             tags$hr(),

             ##action button
             actionButton("mallet_semantic", "Mallet Semantic Network", class = "btn-primary")
           ),


           # Main panel for displaying outputs
           mainPanel(

             # Output: Data file
             plotOutput("mallet_semantic_plot",width = "100%")

           )
         )
)

##mallet semantic network -----------------------
mallet_edges <- eventReactive(input$mallet_semantic,{
  topic.words <- mallet.topic.words(mallet_df(), smoothed = T, normalized = T)

  tmp.1 <- list()
  for(j in 1:nrow(topic.words)){
    topic.df <- mallet.top.words(mallet_df(), topic.words[j,],nrow(t(topic.words)))
    topic.df <- topic.df[topic.df$words != "cultura",]
    topic.df <- topic.df[topic.df$weight >= input$degree_weight,]
    tmp <- list()
    for(i in 1:nrow(topic.df )){
      from <- topic.df[i,1]
      to <- topic.df[topic.df$words != from,]$words
      weight <- list()
      for(z in 1:length(to)){
        weight.1 <- topic.df[i,2]
        weight.2 <- topic.df[topic.df$words == to[z],]$weights
        weight.3 <- weight.1+weight.2
        weight.3 <- weight.3/2
        weight[[z]] <- weight.3/nrow(topic.df)
      }
      weight <- unlist(weight)
      tmp[[i]] <- data.frame(from = from, to = to, weight = weight)
    }
    tmp <- do.call("rbind", tmp)
    tmp$topic <- paste("topic",j)
    tmp.1[[j]] <- tmp
  }

  tmp.1 <- do.call("rbind", tmp.1)
  tmp.1$topic <- factor(tmp.1$topic, levels = unique(tmp.1$topic))

  tmp.1
})

mallet_nodes <- reactive({
  unique(c(as.character(mallet_edges()$from), as.character(mallet_edges()$to)))
})

output$mallet_semantic_plot <- renderPlot({
  net <- igraph::graph_from_data_frame(mallet_edges(), directed=TRUE, vertices= mallet_nodes())
  ## make a distance
  V(net)$degree <- degree(net, mode="all")
  net <- induced_subgraph(net, which(V(net)$degree> input$igraph_semantic_degree))
  l.1 <- igraph::layout_nicely(net)## change the layout
  deg <- igraph::degree(net, mode = "all")

  colrs <- unique(c(brewer.pal(12, "Set3"), brewer.pal(12, "Paired"), brewer.pal(8, "Dark2"),brewer.pal(9, "Pastel1")))

  igraph::plot.igraph(net,
                      layout = l.1,
                      vertex.color = "#F8F7F2",
                      vertex.size = deg*input$igraph_semantic_vertex,
                      vertex.frame.color = "#F8F7F2",
                      vertex.label.color = "#538797",
                      vertex.label.cex = deg*input$igraph_semantic_label,
                      edge.arrow.size = 0,
                      edge.color = colrs[as.numeric(as.factor(E(net)$topic))],
                      edge.width = as.numeric(as.factor(E(net)$weight))*0.001,
                      edge.lty = 1
  )
  title("mallet semantic network across topic",cex.main=1.5,col.main="#6B8B8D")
  legend("bottomleft",levels(as.factor(E(net)$topic)), pch=21, col=colrs, pt.bg=colrs, pt.cex=2, cex=.6, bty="n", ncol=1, horiz = F)
})

