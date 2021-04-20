\name{sentiment_radarmap}
\alias{sentiment_radarmap}
\title{NRC sentiment analysis radarmap}
\usage{
sentiment_radarmap(sentiment_df)
}
\description{
This function drows sentiment radar map.
}
\arguments{
  \item{sentiment_df}{The sentiment data frame from tidytext}

}

\examples{

sentiment_df <- twitter_sentiment(df, language, undesirable_words)

sentiment_radarmap(sentiment_df)
}
\source{
Silge and Robinson (2016) tidytext: Text Mining and Analysis Using Tidy Data Principles in R

Saif Mohammad and Peter Turney (2013) Crowdsourcing a Word-Emotion Association Lexicon, Computational Intelligence, 29 (3), 436-465
}
