library(dplyr)
library(stringr)

text_delete <- function(df, removewords){
  df <- df %>%
  mutate(text = str_remove_all(text, regex(str_c("\\b",removewords, "\\b", collapse = '|'), ignore_case = T)))

  return(df)
}

