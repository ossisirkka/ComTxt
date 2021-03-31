\name{sentiment_semantic}
\alias{Semantic network for sentiment words}
\title{Semantic network for sentiment words}
\usage{
sentiment_semantic(sentiment_df, select = "anticipation", n_nodes = 200)
}
\description{
This function drows semantic network plot of sentiment words.
}
\arguments{
  \item{sentiment_df}{The sentiment data frame from tidytext}

  \item{select}{The NRC sentiment lexicon categories, "anticipation", "anger", "surprise", "joy", "fear", "sadness"}

  \item{n_nodes}{The number of nodes in igraph}
}

\examples{

sentiment_df <- twitter_sentiment(df, language, undesirable_words)

sentiment_semantic(sentiment_df, select = "anticipation", n_nodes = 200)
}
\source{
Silge and Robinson (2016) tidytext: Text Mining and Analysis Using Tidy Data Principles in R

Saif Mohammad and Peter Turney (2013) Crowdsourcing a Word-Emotion Association Lexicon, Computational Intelligence, 29 (3), 436-465
}
