\name{shiny_keyword}
\alias{shiny_keyword}
\title{shiny Keywords Semantic network plot}
\usage{
shiny_keyword(df)
}
\description{
This function retrives semantic network plot in shiny.
}
\arguments{
  \item{df}{The preprocessed twetter data}

}

\examples{
df_pre <- twitter_preprocess(twitter_df, ud_lang = "spanish", stopwords_lang = "es")
shiny_keyword(df_pre)
}
\source{
Benoit et al (2018) quanteda: An R package for the quantitative analysis of textual data
}
