\name{twitter_sentiment}
\alias{Sentiment Analysis}
\title{NRC sentiment analysis}
\usage{
twitter_sentiment(df, language, undesirable_words)
}
\description{
This function is to get NRC sentiment lexicons using tidytext package. The function joins one column of words and another column of categories of lexicon. NRC provides 105 different langauge lexicon
}
\arguments{
  \item{df}{The preprocessed twitter data frame}

  \item{language}{The language set}

  \item{undesirable_words}{The set of words to eliminate for the analysis}
}

\examples{

df_sent <- twitter_sentiment(df_pre, language = "es", undesirable_words = c("lol", "wtf"))

}
\source{
Silge and Robinson (2016) tidytext: Text Mining and Analysis Using Tidy Data Principles in R

Saif Mohammad and Peter Turney (2013) Crowdsourcing a Word-Emotion Association Lexicon, Computational Intelligence, 29 (3), 436-465
}
