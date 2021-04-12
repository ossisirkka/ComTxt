\name{mention_table}
\alias{mention_table}
\title{Top mention communicators}
\usage{
mention_table(df, top_n = 10)
}
\description{
This function retrives top features key word.
}
\arguments{
  \item{df}{The preprocessed twetter data}

  \item{top_n}{top n communicator}

}

\examples{
df_pre <- twitter_preprocess(twitter_df, ud_lang = "spanish", stopwords_lang = "es")
mention_table(df, top_n = 10)
}
\source{
Benoit et al (2018) quanteda: An R package for the quantitative analysis of textual data
}
