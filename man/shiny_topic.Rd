\name{shiny_topic}
\alias{shiny_topic}
\title{shiny topic word plot & radar plot}
\usage{
shiny_topic(mallet_df, df)
}
\description{
This function illustrates word plot of topic and radar chart in shiny.
}
\arguments{
  \item{mallet_df}{mallet_df}
  \item{df}{preprocessed_df}
}

\examples{
df_pre <- twitter_preprocess(twitter_df, ud_lang = "spanish", stopwords_lang = "es")
df_topics <- twitter_topic(df_pre, n_topic = 7)

shiny_topic(df_topics, df_pre)
}

