\name{topic_number}
\alias{topic_number}
\title{The Matrix of Words and Weight for Topics}
\usage{
topic_number(df)
}
\description{
This function retrive the best topic modelling K by HarmonicMean, Coherenc, and perplexity score
the algorithm will calculate a  HarmonicMean, Coherenc, and perplexity score score to allow us to choose the best topics from 1 to k. The scoure presents the probabilistic coherence of each topic. This gives us the quality of the topics being produced.
}
\arguments{
  \item{df}{The preprocessed twitter data}
}

\examples{
df_ pre <- twitter_preprocess(df_tweets, ud_lang = "spanish", stopwords_lang = "es")
topic_number(df)
}
\source{
David Mimno (2013) mallet: A wrapper around the Java machine learning tool MALLET
}
