
library(quanteda.textplots)## Semantic network
library(quanteda)
library(RColorBrewer)
library(dplyr)


mention_network <- function(df, n_top = 50){


  dd <- lapply(df$mentions_screen_name, data.frame)
  tmp <- list()
  for(i in 1:length(dd)){
    tmp[[i]] <- data.frame(from = df$screen_name[[i]], to = dd[[i]])
  }

 tmp <- do.call(rbind.data.frame, tmp)
 colnames(tmp) <- c("from", "to")
 df.network <- tmp

 tmp.wg <- list()
  for(i in 1:nrow(df.network)) {
    tmp.w <- nrow(tmp[tmp$from == tmp$from[i] | tmp$to == tmp$from[i] ,]) + nrow(tmp[tmp$from == tmp$to[i] | tmp$to == tmp$to[i] ,])
    tmp.wg[[i]] <- tmp.w
  }
 tmp[tmp$from == tmp$from[1] | tmp$to == tmp$from[1] ,]
  df.network$weight <- unlist(tmp.wg)
  #df.edges <- df.network
  df.edges <- df.network[df.network$weight >= input$num_communication,]

  df.edges

  edges <- df_edges()
  nodes.1 <- as.data.frame(c(edges$from,edges$to))
  nodes.2 <- nodes.1[!duplicated(nodes.1[,1]), ]
  nodes <- data.frame(nodes.2)
  net <- igraph::graph_from_data_frame(d = edges, directed = T, vertices = nodes)

  plot.igraph(net,
              layout = layout_nicely(net),
              vertex.color = "#F8F7F2",
              vertex.size = degree(net, mode="all")*input$size_vertex,
              vertex.frame.color = "gray",
              #vertex.label = NA,
              vertex.label.color = "#538797",
              vertex.label.cex = degree(net, mode="all")*input$size_vertex_label,
              vertex.label.dist= 2,
              edge.arrow.size = 0.1,
              edge.color = colrs[as.numeric(as.factor(E(net)$type))],
              edge.width = 0.1,
              edge.lty = 2,
              asp = 0.4,
              margin = 0,
  )
  title("[retweet, reply, mention network]",cex.main=1.5,col.main="#6B8B8D")
  legend("bottomleft",levels(as.factor(E(net)$type)), pch=21, col=colrs, pt.bg=colrs, pt.cex=2, cex=.6, bty="n", ncol=1, horiz = T)


  ##------------
  for(i in 1:nrow(df)){
    df$mentions_screen_name[[i]] <- tolower(paste(paste0("@", df$mentions_screen_name[[i]]), collapse = " "))
  }
  df$mentions_screen_name <- gsub("#na", NA, df$mentions_screen_name)
  df <- df[!is.na(df$mentions_screen_name),]

  hash_dfm <- dfm(df$mentions_screen_name)

  toptag <- names(topfeatures(hash_dfm, n_top)) # Most important mentions_screen_name; we dont want to plot every hashtag
  tag_fcm <- fcm(hash_dfm) # Feature-occurance matrix which shows the network structure
  topgat_fcm <- fcm_select(tag_fcm, pattern = toptag) # Filter results so that we plot only 50 top mentions_screen_name

  ##draw semantic network plot
  quanteda::textplot_network(topgat_fcm, min_freq = 1, edge_color = "grey",vertex_color ="#538797")
}









