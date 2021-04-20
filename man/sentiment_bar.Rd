\name{sentiment_bar}
\alias{sentiment_bar}
\title{NRC sentiment analysis bar plot}
\usage{
sentiment_bar(sentiment_df, by)
}
\description{
This function drows bar sentiment chart.
}
\arguments{
  \item{sentiment_df}{The sentiment data frame from tidytext}

  \item{by}{the created time divided by year, or by month}

}

\examples{
df_sent <- twitter_sentiment(df_pre, language = "es", undesirable_words = c("lol", "wtf"))
sentiment_bar(df_sent)
}
\source{
Silge and Robinson (2016) tidytext: Text Mining and Analysis Using Tidy Data Principles in R

Saif Mohammad and Peter Turney (2013) Crowdsourcing a Word-Emotion Association Lexicon, Computational Intelligence, 29 (3), 436-465
}
