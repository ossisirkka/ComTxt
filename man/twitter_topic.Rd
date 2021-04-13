\name{get_topic}
\alias{Mallet Topic Modelling}
\title{Mallet Topic Modelling On Twitter Data}
\usage{
twitter_topic(df, n_topic)
}
\description{
This function allows you to train Mallet topic modelling on Twitter data using Mallet pacakge (Mimno, 2013).  The result of Topic modelling display directly R and Shiny.
}
\arguments{
  \item{df}{The preprocessed Twitter data frame. The column names must include "status_id", "text"}

  \item{n_topic}{The numbers of topics}
}

\examples{
df_ pre <- twitter_preprocess(df_tweets, ud_lang = "spanish", stopwords_lang = "es")
df_topics <- twitter_topic(df_pre, n_topic = 7)

}
\source{
David Mimno (2013) mallet: A wrapper around the Java machine learning tool MALLET
}
