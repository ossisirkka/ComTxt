\name{shiny_mention}
\alias{shiny_mention}
\title{shiny mention & user table and network plot}
\usage{
shiny_mention(df)
}
\description{
This function retrives semantic network plot in shiny.
}
\arguments{
  \item{df}{The preprocessed twetter data}

}

\examples{
df_pre <- twitter_preprocess(twitter_df, ud_lang = "spanish", stopwords_lang = "es")
shiny_mention(df_pre)
}
