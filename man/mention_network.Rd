\name{mention_network}
\alias{mention_network}
\title{mention network plot}
\usage{
mention_network(df, n_top = 40)
}
\description{
This function retrives hashtags semantic network plot.
}
\arguments{
  \item{df}{The preprocessed twetter data}

  \item{n_top}{top n tokens}

}

\examples{
df_pre<- twitter_preprocess(twitter_df, ud_lang = "spanish", stopwords_lang = "es")
mention_network(df_pre, n_top = 40)
}
\source{
Benoit et al (2018) quanteda: An R package for the quantitative analysis of textual data
}
