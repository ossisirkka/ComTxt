library(dplyr) #Data manipulation (also included in the tidyverse package)
library(tidytext) #Text mining
library(tidyr) #Spread, separate, unite, text mining (also included in the tidyverse package)
library(widyr) #Use for pairwise correlation
library(textdata)


twitter_sentiment <- function(df, language, undesirable_words){
  stopwords <- data.frame(word = stopwords::stopwords(language = language, source = "stopwords-iso"))
  stopwords$lexicon <- language
  stopwords <- as_tibble(stopwords)
  #Create tidy text format: Unnested, Unsummarized, -Undesirables, Stop and Short words
  nrc_tidy <- df %>%
  unnest_tokens(word, text) %>% #Break the lyrics into individual words
  filter(!word %in% undesirable_words) %>% #Remove undesirables
  filter(!nchar(word) < 3) %>% #Words like "ah" or "oo" used in music
  anti_join(stopwords)

  youtube_nrc <- nrc_tidy %>%
  inner_join(get_sentiments("nrc"))
}


