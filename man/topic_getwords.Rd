\name{topic_getwords}
\alias{topic_getwords}
\title{The Matrix of Words and Weight for Topics}
\usage{
topic_getwords(mallet_df, n_topic, n_words)
}
\description{
This function retrive the matrix of words and its weight for topics.
}
\arguments{
  \item{mallet_df}{The model trained by get_topic function}

  \item{n_topic}{The numbers of topics for the model}

  \item{n_words}{The number of words for the matrix}
}

\examples{
df_ pre <- twitter_preprocess(df_tweets, ud_lang = "spanish", stop_words)
df_topics <- twitter_topic(df_pre, n_topic = 7)
words <- topic_getwords(df_topics, n_words = 100)
}
\source{
David Mimno (2013) mallet: A wrapper around the Java machine learning tool MALLET
}
