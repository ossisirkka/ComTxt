\name{keyword_df}
\alias{keyword_df}
\title{Top keywords data frame}
\usage{
keyword_df(df, top_n = 10)
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
keyword_df(df, top_n = 10)
}
\source{
Benoit et al (2018) quanteda: An R package for the quantitative analysis of textual data
}
