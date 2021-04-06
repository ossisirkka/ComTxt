## example

library(devtools)
install_github("BeaJJ/ComTxt")

library(ComTxt)

## preprocess : lemmatization, stopwords, urls, emoticons
df_pre <- twitter_preprocess(twitter_df, ud_lang = "spanish", stopwords_lang = "es")

## Topic modeling------------------------------------------------------
### mallet topic modeling
df_topics <- twitter_topic(df_pre, language = "es", n_topic = 7)

### data frame words topic
words <- topic_getwords(df_topics, n_topic = 7, n_words = 100)

### draw wordscloud for each topics 3*3 = 9 spots (depending on your topic numbers)
topic_wordcloud(df_topics, par_row = 3, par_col = 3, n_topic = 7, maximum_n = 200, cloud_scale = 2.5)

### draw topic semantic networks
topic_network(df_topics, n_nodes = 50, igraph_semantic_label= 0.007)


## semantic network ----------------------------------------
### semantic general
semantic_network(df_pre,language = "es", n_topic = 40, vertext_size = 0.5)

### semantic hub network
semantic_network_hub(df_pre, language = "es", hub = "arte")

## Sentiment analysis ------------------------------
### NRC sentiment analysis
df_sent <- twitter_sentiment(df_pre, language = "es", undesirable_words = c("lol", "wtf"))

### sentiment bar plot
sentiment_bar(df_sent)

### sentiment wordsplot
sentiment_wordsplot(df_sent)

### sentiment circos plot
sentiment_circos(df_sent)

### sentiment radarmap
sentiment_radarmap(df_sent)

### sentiment semantic network
sentiment_semantic(df_sent, select = "anticipation", n_nodes = 200)









