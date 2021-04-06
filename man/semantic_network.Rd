\name{semantic_network}
\alias{semantic network}
\title{Semantic network plot}
\usage{
semantic_network(df, language = "en", n_top = 40, vertext_size = 0.5)
}
\description{
This function retrives semantic network plot.
}
\arguments{
  \item{df}{The preprocessed twetter data}

  \item{language}{stopwords language}

  \item{n_top}{top n tokens}

  \item{vertext_size}{the size of vertext in network graph}
}

\examples{
df_pre <- twitter_preprocess(twitter_df, ud_lang = "spanish", stopwords_lang = "es")
semantic_network(df_pre, language = "en", n_top = 40, vertext_size = 0.5)
}
\source{
Benoit et al (2018) quanteda: An R package for the quantitative analysis of textual data
}
