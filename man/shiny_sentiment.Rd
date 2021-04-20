\name{shiny_sentiment}
\alias{shiny_sentiment}
\title{shiny sentiment word plot & radar plot}
\usage{
shiny_sentiment(df)
}
\description{
This function illustrates word plot of topic and radar chart in shiny.
}
\arguments{
  \item{df}{sentiment data frame}
}

\examples{
df_pre <- twitter_preprocess(twitter_df, ud_lang = "spanish", stopwords_lang = "es")
df_sent <- twitter_sentiment(df_pre, language = "es", undesirable_words = "shit")
shiny_sentiment(df_sent)
}

