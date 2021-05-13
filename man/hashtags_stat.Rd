\name{hashtag_stat}
\alias{hashtag_stat}
\title{hashtag statistics}
\usage{
hashtag_stat(df)
}
\description{
This function retrives hashtags statistic info.
}
\arguments{
  \item{df}{The preprocessed twetter data}

}

\examples{
df_pre <- twitter_preprocess(twitter_df, ud_lang = "spanish", stopwords_lang = "es")
hashtag_stat(df)
}

