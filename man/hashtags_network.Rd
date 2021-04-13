\name{hashtags_network}
\alias{hashtags_network}
\title{hashtags Semantic network plot}
\usage{
hashtags_network(df,  hashtag_remove, top_n = 40)
}
\description{
This function retrives hashtags semantic network plot.
}
\arguments{
  \item{df}{The preprocessed twetter data}

  \item{hashtag_remove}{hashtag_remove to remove}

  \item{top_n}{top n tokens}

}

\examples{
df_pre <- twitter_preprocess(twitter_df, ud_lang = "spanish", stopwords_lang = "es")
hashtags_network(df_pre,  hashtag_remove = "#cultura", top_n = 40)
}
\source{
Benoit et al (2018) quanteda: An R package for the quantitative analysis of textual data
}
