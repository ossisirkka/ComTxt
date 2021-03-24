## Sentiment analysis NRC
## data processing holy shit bullshit fucking
library(syuzhet)

sent_df <- pre_df
sent_df$textOriginal <- gsub("holy shit", " ", sent_df$textOriginal, perl=T)
sent_df$textOriginal <- gsub("bullshit", " ", sent_df$textOriginal, perl=T)
sent_df$textOriginal <- gsub("fucking", " ", sent_df$textOriginal, perl=T)
sent_df$textOriginal <- gsub("[^',[:^punct:][:blank:]]+", " ", sent_df$textOriginal, perl=T)
sent_df$textOriginal <- gsub('[0-9]+', '', sent_df$textOrigina)

sent_df$textOriginal <- paste0(gsub('\\.', '', sent_df$textOriginal), '.')

s_v <- get_sentences(sent_df$textOriginal)

## Sentiment plot
s_v_sentiment <- get_sentiment(s_v,method = "nrc", lang = "english")
sent_df$sent <- s_v_sentiment
plot(
  s_v_sentiment,
  type="l",
  main="Example Plot Trajectory",
  xlab = "Narrative Time",
  ylab= "Emotional Valence"
)

## normalize
percent_vals <- get_percentage_values(s_v_sentiment, bins = 13)
plot(
  percent_vals,
  type="l",
  main="Sentiment Using Percentage-Based Means",
  xlab = "Narrative Time",
  ylab= "Emotional Valence",
  col="red"
)

##Shape smoothing and normalization using a Fourier based transformation and low pass filtering is achieved using the get_transformed_values function as shown below.

ft_values <- get_transformed_values(
  s_v_sentiment,
  low_pass_size = 3,
  x_reverse_len = 100,
  padding_factor = 2,
  scale_vals = TRUE,
  scale_range = FALSE
)

plot(
  ft_values,
  type ="l",
  main ="Joyce's Portrait using Transformed Values",
  xlab = "Narrative Time",
  ylab = "Emotional Valence",
  col = "red"
)

simple_plot(s_v_sentiment)

## NRC data
nrc_data <- get_nrc_sentiment(s_v)
sent_df <- cbind(sent_df, nrc_data)


saveRDS(sent_df, file = "Directory/playground/data/sent_df.Rda")


df<- list()
for(i in 1:13){
  sent_df_1 <- sent_df[sent_df$ID == i, ]
  df_1 <- colSums(prop.table(sent_df_1[, 15:22]))
  df_1 <- data.frame(prob = df_1)
  df_1 <- data.frame(emotion= rownames(df_1), df_1)
  rownames(df_1) <- NULL
  df[[i]] <- cbind(ID = unique(sent_df_1$videoId), df_1)
}

df_nrc <- do.call(rbind, df)
saveRDS(df_nrc, "Directory/playground/data/df_nrc.Rda")
df_nrc$ID <- factor(df_nrc$ID, levels = unique(df_nrc$ID))

ggplot(df_nrc, aes(x=ID, y=prob, group=emotion)) +
  geom_line(aes(color=emotion), size =2)+
  #geom_point(aes(color=emotion))+
  theme_classic()+
  scale_x_discrete(name ="VideoID")+
  theme(axis.text.x = element_text(face="bold", size=10, angle=45))

df<- list()
for(i in 1:13){
  sent_df_1 <- sent_df[sent_df$ID == i, ]
  df_1 <- colSums(prop.table(sent_df_1[, 23:24]))
  df_1 <- data.frame(prob = df_1)
  df_1 <- data.frame(emotion= rownames(df_1), df_1)
  rownames(df_1) <- NULL
  df[[i]] <- cbind(ID = unique(sent_df_1$videoId), df_1)
}

df_pol <- do.call(rbind, df)

df_pol$ID <- factor(df_pol$ID, levels = unique(df_pol$ID))

ggplot(df_pol, aes(x=ID, y=prob, group=emotion)) +
  geom_line(aes(color=emotion, linetype = emotion), size =2)+
  #geom_point(aes(color=emotion))+
  theme_classic()+
  scale_x_discrete(name ="VideoID")+
  theme(axis.text.x = element_text(face="bold", size=10, angle=45))

sent_df$sent <- rowSums(sent_df[,15:24])

nrow(sent_df[sent_df$sent == 0,])/nrow(sent_df)*100

library(tidyverse)
Anger <- sent_df %>% arrange(desc(anger))
head(anger)


joy <- sent_df %>% arrange(desc(joy))
head(joy)

anticipation <- sent_df %>% arrange(desc(anticipation))
head(anticipation)

fear <- sent_df %>% arrange(desc(fear))
head(fear)

disgust <- sent_df %>% arrange(desc(disgust))
head(disgust)

sadness <- sent_df %>% arrange(desc(sadness))
head(sadness)

trust <- sent_df %>% arrange(desc(trust))
head(trust)

surprise <- sent_df %>% arrange(desc(surprise))
head(surprise)


library("writexl")
write_xlsx(sent_df,"sent_df.xlsx")
