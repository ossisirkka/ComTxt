\name{mention_stat}
\alias{mention_stat}
\title{mention statistics}
\usage{
mention_stat(df)
}
\description{
This function retrives mention statistic info.
}
\arguments{
  \item{df}{The preprocessed twetter data}

}

\examples{
df_pre <- twitter_preprocess(twitter_df, ud_lang = "spanish", stopwords_lang = "es")
mention_stat(df)
}

