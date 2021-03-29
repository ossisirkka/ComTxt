\name{get_topic}
\alias{Mallet Topic Modelling}
\title{Mallet Topic Modelling On Twitter Data}
\usage{
get_topic(df, language, n_topic)
}
\description{
This function allows you to train Mallet topic modelling on Twitter data using Mallet pacakge (Mimno, 2013).  The result of Topic modelling display directly R and Shiny.
}
\arguments{
  \item{df}{The Twitter data frame. The column names must include "status_id", "text"}

  \item{language}{The langauge of data}

  \item{n_topic}{The numbers of topics}
}

\examples{
library(ComTxt)
library(rtweet)
basic_tweets <- search_tweets("hashtags", 100)

topics <- get_topic(basic_tweets, language = "es", n_topic = 7)

}
\source{
David Mimno (2013) mallet: A wrapper around the Java machine learning tool MALLET
}
