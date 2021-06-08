\name{topics_trend}
\alias{topics_trend}
\title{Topic distribution over the period by month}
\usage{
topics_trend(mallet_df, df, col = "royalblue")
}
\description{
This function display the topic trend by month
}
\arguments{
  \item{mallet_df}{mallet topic model}

  \item{df}{preprocessed twitter data}

  \item{topic_name}{topic names}

}

\examples{

#### Example ####
topic_name <- c("Topic 1", "topic 2", "c", "d")

topics_trend(mallet_df, # model
             df, # dataframe
             topic_name) # label of the topics

}
