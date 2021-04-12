\name{keyword_table}
\alias{keyword_table}
\title{Top keywords data frame}
\usage{
keyword_table(df, top_n = 10)
}
\description{
This function retrives top features key word.
}
\arguments{
  \item{df}{The preprocessed twetter data}

  \item{keyword_remove}{keyword to remove}

  \item{top_n}{top n keywords}

}

\examples{
df_pre <- twitter_preprocess(twitter_df, ud_lang = "spanish", stopwords_lang = "es")
keyword_table(df, keyword_remove = "cultura", top_n = 10)
}
\source{
Benoit et al (2018) quanteda: An R package for the quantitative analysis of textual data
}
