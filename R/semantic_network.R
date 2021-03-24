## Semantic network

library(quanteda)
library(RColorBrewer)
library(wordcloud)
library(dplyr)
library(data.table)


##Subset the data language
language_set = "en"
question_id = "Q.3" ##"Q.4" "Q.5", "Q.6"

df <- raw_uk_df[raw_uk_df$Language == language_set & raw_uk_df$Question_Number == question_id, ]

##delete the emoticon and word "etc"
df$Answer <- gsub("[<]+.*[>]", "", df$Answer)

toks <- tokens(df$Answer, remove_punct = TRUE, remove_symbols = TRUE, verbose = TRUE)
toks <- tokens_tolower(toks)
toks <- tokens_remove(toks, pattern = stopwords::stopwords(language_set, source = "stopwords-iso"), padding = FALSE, min_nchar =3)
toks <- tokens_wordstem(toks, language = language_set)

## A feature co-occurrence matrix
fcmat <-quanteda::fcm(toks, context = "window", tri = FALSE)

##reduce only top words
feat <-  names(topfeatures(fcmat, 40))

## draw the table
data_fcmat <- data.frame(name = rownames(fcmat))
rownames(data_fcmat) <- NULL
data_fcmat$degree <- rowSums(fcmat)
table_fcmat <- data_fcmat
##see the tale
fcm_tb <- table_fcmat[table_fcmat$name %in% feat, ]

fcm_tb <- fcm_tb %>% arrange(desc(degree))
##subset top 40 words
fcm_1 <- fcm_select(fcmat, pattern = feat)
##draw semantic network plot
quanteda::textplot_network(fcm_1, min_freq = 0.1, edge_color = "grey",vertex_color ="#538797",  vertex_labelsize = (rowSums(fcm_1)/min(rowSums(fcm_1)))*0.5, edge_size = 3)


