\name{topic_words}
\alias{Mallet Topic Modelling}
\title{The Matrix of Words and Weight for Topics}
\usage{
topic_words(mallet_df, n_topic, n_words)
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


topics <- get_topic(basic_tweets, language = "es", n_topic = 7)

words <- topic_words(topics, n_topic = 7, n_words = 100)
}
\source{
David Mimno (2013) mallet: A wrapper around the Java machine learning tool MALLET
}
