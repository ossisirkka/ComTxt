\name{topic_radarmap}
\alias{topic_radarmap}
\title{Topic distribution by year}
\usage{
topic_radarmap(df ,mallet_df, n_topic)
}
\description{
This function display word term plot of topics.
}
\arguments{
  \item{df}{preprocessed twitter data}

  \item{mallet_df}{mallet topic model}

  \item{n_topic}{The numbers topics of model}

}

\examples{

df_pre <- twitter_preprocess(df_tweets, ud_lang = "spanish", stopwords_lang = "es")
df_topics <- twitter_topic(df_pre, language = "es", n_topic = 7)
topic_radarmap(df_pre ,df_topics, n_topic =7)

}
\source{
David Mimno (2013) mallet: A wrapper around the Java machine learning tool MALLET
}
