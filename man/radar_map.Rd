\name{topic_words}
\alias{Mallet Topic Modelling}
\title{The Matrix of Words and Weight for Topics}
\usage{
radar_map(sentiment_df)
}
\description{
This function drows sentiment radar map.
}
\arguments{
  \item{sentiment_df}{The sentiment data frame from tidytext}

}

\examples{

sentiment_df <- twitter_sentiment(df, language, undesirable_words)

bar_sentiment(sentiment_df)
}
\source{
Silge and Robinson (2016) tidytext: Text Mining and Analysis Using Tidy Data Principles in R

Saif Mohammad and Peter Turney (2013) Crowdsourcing a Word-Emotion Association Lexicon, Computational Intelligence, 29 (3), 436-465
}
