## local hub
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

##selecting keywords inside fcmat
tmp <- fcmat[, "art"]
tmp <- as.data.table(tmp)
colnames(tmp) <- c("nodes", "degree")
tmp <- tmp[tmp$degree > 0, ]

## deleteing the higher degree nodes then key word degree
nodes_degree <- table_fcmat[table_fcmat$name == "art", ]$degree
high_nodes <- table_fcmat[table_fcmat$degree > nodes_degree, ]$name

if(high_nodes %in% tmp$nodes  == TRUE){
  tmp <- tmp[ !tmp$nodes %in% high_nodes, ]$nodes
} else {
  tmp <- tmp$nodes
}
fcm_local <- fcm_select(fcmat, pattern = c(tmp, "art"))

##text plot
quanteda::textplot_network(fcm_local, min_freq = 0.1, edge_color = "grey",vertex_color ="#538797",  vertex_labelsize = (rowSums(fcm_local)/min(rowSums(fcm_local)))*0.3, edge_size = 3)
