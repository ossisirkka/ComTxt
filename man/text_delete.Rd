\name{text_delete}
\alias{text_delete}
\title{Preprocess Twitter textual data}
\usage{
text_delete(df,removewords)
}
\description{
This function retrives lemma version of text data.
}
\arguments{
  \item{df}{data frame}

  \item{removewords}{Not add any meaning words}
}
\value{
  \item{df}{data frame}

  \item{removewords}{add all the not necessary words}

}
\examples{
df_pre <- twitter_preprocess(df_tweets, ud_lang = "spanish", stop_words)
removewords <- c("not", "any")
df_pre <- text_delete(df,removewords)
}

