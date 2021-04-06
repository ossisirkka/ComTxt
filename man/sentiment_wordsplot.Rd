\name{words_sentiment}
\alias{Sentiment Words Plot}
\title{Sentiment Words plot}
\usage{
sentiment_wordsplot(sentiment_df)
}
\description{
This function get the sentiment words plot.
}
\arguments{
  \item{sentiment_df}{The sentiment df}
}

\examples{
sentiment_df <- twitter_sentiment(df_pre, language = "en", undesirable_words = c("lol", "shit"))
sentiment_wordsplot(sentiment_df)
}
\source{
Silge and Robinson (2016) tidytext: Text Mining and Analysis Using Tidy Data Principles in R

Saif Mohammad and Peter Turney (2013) Crowdsourcing a Word-Emotion Association Lexicon, Computational Intelligence, 29 (3), 436-465
}
