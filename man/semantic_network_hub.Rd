\name{semantic_network_hub}
\alias{Semantic network hub}
\title{Semantic network hub}
\usage{
semantic_network_hub(df, language, hub)
}
\description{
This function draws zoomed in semantic network around hub.
}
\arguments{
  \item{df}{The preprocessed twitter data}

  \item{language}{stopwords language "es"}

  \item{hub}{zoomed in word}
}

\examples{

df_pre <- twitter_preprocess(twitter_df, ud_lang = "spanish", stopwords_lang = "es")
semantic_network_hub(df_pre, language = "es", hub = "arte")
}
\source{
Benoit et al (2018) quanteda: An R package for the quantitative analysis of textual data
}
