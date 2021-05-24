\name{given_model_K}
\alias{given_model_K}
\title{The Matrix of Words and Weight for Topics}
\usage{
given_model_K(df, mallet_df)
}
\description{
This function retrive the  perplexity score
}
\arguments{
  \item{df}{The preprocessed twitter data}

  \item{mallet_df}{mallet_df}
}

\examples{
df_pre <- twitter_preprocess(df_tweets, ud_lang = "spanish", stopwords_lang = "es")
df_topic <- twitter_topic(df_pre, 10)
given_model_K(df_pre, df_topic)
}

