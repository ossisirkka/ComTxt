\name{topic_wordcloud}
\alias{Mallet Topic Modelling}
\title{The wordcloud plot for topics}
\usage{
topic_wordcloud(mallet_df, par_row, par_col, n_topic, maximum_n, cloud_scale )
}
\description{
This function display wordclouds plot of topics.
}
\arguments{
  \item{mallet_df}{The model trained by get_topic function}

  \item{par_row}{The numbers of rows in plot. default 3}

  \item{par_col}{The number of coulmn in plot. default 3}

  \item{n_topic}{The number of topics in the model}

  \item{maximum_n}{The maximum number of words per clouds. default 200}

  \item{cloud_scale}{The size of wordclouds. default 2.5}
}

\examples{
library(ComTxt)

topics <- get_topic(basic_tweets, language = "es", n_topic = 7)

topic_wordcloud(topics, par_row = 3, par_col = 3, n_topic = 7, maximum_n = 200, cloud_scale = 2.5)

}
\source{
David Mimno (2013) mallet: A wrapper around the Java machine learning tool MALLET
}
