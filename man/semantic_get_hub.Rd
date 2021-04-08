\name{semantic_get_hub}
\alias{semantic get_hub}
\title{Semantic get_hub}
\usage{
semantic_get_hub(df)
}
\description{
This function gets top 20 nodes in semantic network.
}
\arguments{
  \item{df}{The preprocessed twetter data}

}

\examples{
df_pre <- twitter_preprocess(twitter_df, ud_lang = "spanish", stopwords_lang = "es")
semantic_get_hub(df)0)
}
