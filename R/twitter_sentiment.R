library(dplyr) #Data manipulation (also included in the tidyverse package)
library(tidytext) #Text mining
library(tidyr) #Spread, separate, unite, text mining (also included in the tidyverse package)
library(widyr) #Use for pairwise correlation
library(textdata)
library(readxl)

twitter_sentiment <- function(df, language, undesirable_words){
  #stopwords <- data.frame(word = stopwords::stopwords(language, source = "stopwords-iso"))
  #stopwords$lexicon <- language
  #stopwords <- as_tibble(stopwords)


  NRC_Emotion_Lexicon <- read_excel("source/NRC-Emotion-Lexicon-v0.92-In105Languages-Nov2017Translations.xlsx")
  names <- colnames(NRC_Emotion_Lexicon)
  names <-gsub(".* ","",names)
  names <-gsub("[()]","",names)

  colnames(NRC_Emotion_Lexicon) <- names
  NRC_Emotion_Lexicon <- NRC_Emotion_Lexicon %>% select(-"en...1")

  custom_lexicon <- NRC_Emotion_Lexicon %>% select(language, Anger, Anticipation, Disgust,Fear, Joy, Sadness, Surprise, Trust)

 colnames(custom_lexicon) <- c("word", "Anger", "Anticipation", "Disgust","Fear", "Joy", "Sadness", "Surprise", "Trust" )

 custom_lexicon <- custom_lexicon[rowSums(custom_lexicon[,2:9]) >0,]
  #Create tidy text format: Unnested, Unsummarized, -Undesirables, Stop and Short words
  nrc_tidy <- df %>%
  unnest_tokens(word, text) %>% #Break the lyrics into individual words
  filter(!word %in% undesirable_words) %>% #Remove undesirables
  filter(!nchar(word) < 3)

  youtube_nrc <- nrc_tidy %>%
  inner_join(custom_lexicon)


  df_1 <- youtube_nrc[,1:91]
  df_1<- df_1[df_1$Anger == 1,]
  df_1$Anger <- "Anger"
  colnames(df_1)[colnames(df_1) == "Anger"] <- "sentiment"

  df_2 <- youtube_nrc[,c(1:90, 92)]
  df_2<- df_2[df_2$Anticipation == 1,]
  df_2$Anticipation <- "Anticipation"
  colnames(df_2)[colnames(df_2) == "Anticipation"] <- "sentiment"

  df_3 <- youtube_nrc[,c(1:90, 93)]
  df_3<- df_3[df_3$Disgust == 1,]
  df_3$Disgust <- "Disgust"
  colnames(df_3)[colnames(df_3) == "Disgust"] <- "sentiment"

  df_4 <- youtube_nrc[,c(1:90, 94)]
  df_4<- df_4[df_4$Fear == 1,]
  df_4$Fear <- "Fear"
  colnames(df_4)[colnames(df_4) == "Fear"] <- "sentiment"

  df_5 <- youtube_nrc[,c(1:90, 95)]
  df_5<- df_5[df_5$Joy == 1,]
  df_5$Joy <- "Joy"
  colnames(df_5)[colnames(df_5) == "Joy"] <- "sentiment"

  df_6 <- youtube_nrc[,c(1:90, 96)]
  df_6<- df_6[df_6$Sadness == 1,]
  df_6$Sadness <- "Sadness"
  colnames(df_6)[colnames(df_6) == "Sadness"] <- "sentiment"

  df_7 <- youtube_nrc[,c(1:90, 97)]
  df_7<- df_7[df_7$Surprise == 1,]
  df_7$Surprise <- "Surprise"
  colnames(df_7)[colnames(df_7) == "Surprise"] <- "sentiment"

  df_8 <- youtube_nrc[,c(1:90, 98)]
  df_8<- df_8[df_8$Trust == 1,]
  df_8$Trust <- "Trust"
  colnames(df_8)[colnames(df_8) == "Trust"] <- "sentiment"

  tmp <- rbind(df_1, df_2, df_3, df_4, df_5, df_6, df_7, df_8)
  tmp <- tmp[order(tmp$created_at),]
  return(tmp)
}


