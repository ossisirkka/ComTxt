library(readxl)
library(dplyr)
library(usethis)
NRC_Emotion_Lexicon <- read_excel("source/NRC-Emotion-Lexicon-v0.92-In105Languages-Nov2017Translations.xlsx")
names <- colnames(NRC_Emotion_Lexicon)
names <-gsub(".* ","",names)
names <-gsub("[()]","",names)

colnames(NRC_Emotion_Lexicon) <- names
NRC_Emotion_Lexicon <- NRC_Emotion_Lexicon %>% select(-"en...1")

usethis::use_data(NRC_Emotion_Lexicon)
