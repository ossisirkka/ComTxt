library(dplyr) #Data manipulation (also included in the tidyverse package)
library(tidytext) #Text mining
library(tidyr) #Spread, separate, unite, text mining (also included in the tidyverse package)
library(widyr) #Use for pairwise correlation
library(textdata)
library(readxl)

twitter_sentiment <- function(df, language, undesirable_words){
  stopwords <- data.frame(word = stopwords::stopwords(language, source = "stopwords-iso"))
  stopwords$lexicon <- language
  stopwords <- as_tibble(stopwords)


  NRC_Emotion_Lexicon <- read_excel("source/NRC-Emotion-Lexicon-v0.92-In105Languages-Nov2017Translations.xlsx")
  names <- colnames(NRC_Emotion_Lexicon)
  names <-gsub(".* ","",names)
  names <-gsub("[()]","",name)

  colnames(NRC_Emotion_Lexicon) <- names
  NRC_Emotion_Lexicon <- NRC_Emotion_Lexicon %>% select(-"en...1")

  custom_lexicon <- NRC_Emotion_Lexicon %>% select(language, Anger, Anticipation, Disgust,Fear, Joy, Sadness, Surprise, Trust)

 colnames(custom_lexicon) <- c("word", "Anger", "Anticipation", "Disgust","Fear", "Joy", "Sadness", "Surprise", "Trust" )

 custom_lexicon <- custom_lexicon[rowSums(custom_lexicon[,2:9]) >0,]
  #Create tidy text format: Unnested, Unsummarized, -Undesirables, Stop and Short words
  nrc_tidy <- df %>%
  unnest_tokens(word, text) %>% #Break the lyrics into individual words
  filter(!word %in% undesirable_words) %>% #Remove undesirables
  filter(!nchar(word) < 3) %>% #Words like "ah" or "oo" used in music
  anti_join(stopwords)

  youtube_nrc <- nrc_tidy %>%
  inner_join(custom_lexicon)
}


