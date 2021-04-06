\name{sentiment_wordsplot}
\alias{Sentiment Analysis}
\title{NRC sentiment analysis wordsplot}
\usage{
sentiment_circos(sentiment_df)
}
\description{
This function drows bar sentiment chart.
}
\arguments{
  \item{sentiment_df}{The sentiment data frame from tidytext}

}

\examples{
df_sent <- twitter_sentiment(df_pre, language = "es", undesirable_words = c("lol", "wtf"))
sentiment_circos(df_sent)
}
\source{
Silge and Robinson (2016) tidytext: Text Mining and Analysis Using Tidy Data Principles in R

Saif Mohammad and Peter Turney (2013) Crowdsourcing a Word-Emotion Association Lexicon, Computational Intelligence, 29 (3), 436-465
}
