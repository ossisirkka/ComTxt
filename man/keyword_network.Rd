\name{keyword_network}
\alias{keyword_network}
\title{Keywords Semantic network plot}
\usage{
keyword_network(df, keyword_remove, top_n = 40)
}
\description{
This function retrives semantic network plot.
}
\arguments{
  \item{df}{The preprocessed twetter data}

  \item{keyword_remove}{search key word remove}

  \item{top_n}{top n tokens}

}

\examples{
df_pre <- twitter_preprocess(twitter_df, ud_lang = "spanish", stopwords_lang = "es")
keyword_network(df_pre,keyword_remove = "cultura", top_n = 40)
}
\source{
Benoit et al (2018) quanteda: An R package for the quantitative analysis of textual data
}
