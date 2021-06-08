##### Packages you need to install/load ####

library(ggplot2)
library(mallet)
library(RColorBrewer)
library(dplyr)
library(reshape2)
library(lubridate)




topics_trend <- function(model, df, topic_name = NULL){

  tmp <- as.data.frame(mallet.doc.topics(model, TRUE))

  cnames <- names(tmp) # name of variables

  nt <- length(cnames) # Number of topics in the model

  Sys.setlocale("LC_TIME", "English")  # Setting dates in english

  tmp$date <- format(as.Date(df$created_at), "%Y/%m") #date of each tweet

  yt <- year(sample(df$created_at, 1))

   if(is.null(topic_name)){
    tp <- paste0("Topic ", c(1:nt))}else{
      if(!is.character(topic_name) | length(topic_name) != nt){
        print("Error: Please check the number of topic's labels")
        stop()
      } else{tp <- topic_name}

    }


   #

  #####
  # Number of tweets
  tmp <- tmp %>% group_by(date) %>% summarise(across(names(tmp)[1]:names(tmp)[nt], sum))


  #tmp <- tmp[c(5,4,8,1,9,7,6,2,12,11,10,3),] # regordening the months

  tmp$datenum <- c(1:length(tmp$date))

  a <- melt(tmp[2:dim(tmp)[2]], id.vars = "datenum")

  tmp$month <- paste0(month(ym(tmp$date), TRUE)) # paste0(month(ym(tmp$date), TRUE), "/", yt
  tmp$month <- paste0(tmp$month, "/",str_sub(tmp$date,1,4))

  ####
    mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(nt)

  graph <- ggplot(a, aes(x = datenum, y = value, colour =variable)) + geom_line(size = 0.8)+
    theme_bw() +
    theme(text=element_text(size=10, family="Garamond", hjust = 0.5, vjust = 0.5),
          legend.title = element_text(colour = "Black",  face = "bold", size = 10),
          plot.title = element_text(size=14, face="bold.italic"),
          legend.position = "right")+
    ggtitle(paste0("Topics trend per month_", yt)) +
    scale_y_continuous("Number of Tweets")+
    scale_x_continuous("", breaks = c(1:length(tmp$date)), labels = tmp$month)+
    scale_color_manual(values = mycolors,labels = tp)+
    guides(color = guide_legend(title="Topic",
                                title.position = "top",
                                title.hjust = 0.5))
 print(graph)
}

















