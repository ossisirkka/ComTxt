\name{user_table}
\alias{user_table}
\title{Top user data frame}
\usage{
user_table(df, top_n = 10)
}
\description{
This function retrives top features key word.
}
\arguments{
  \item{df}{The preprocessed twetter data}

  \item{top_n}{top n keywords}

}

\examples{
df_pre <- twitter_preprocess(twitter_df, ud_lang = "spanish", stopwords_lang = "es")
user_table(df, top_n = 10)
}
\source{
Benoit et al (2018) quanteda: An R package for the quantitative analysis of textual data
}
