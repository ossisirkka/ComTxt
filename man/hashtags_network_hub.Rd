\name{hashtags_network_hub}
\alias{hashtags_network_hub}
\title{hashtags Semantic network hub}
\usage{
hashtags_network_hub(df, hub, top_n = 40)
}
\description{
This function draws zoomed in semantic network around hub.
}
\arguments{
  \item{df}{The preprocessed twitter data}

  \item{hub}{zoomed in word}

  \item{top_n}{The number of nodes in the plot}
}

\examples{

df_pre <- twitter_preprocess(twitter_df, ud_lang = "spanish", stopwords_lang = "es")
hashtags_network_hub(df_pre, hub = "#art", top_n = 40)
}
\source{
Benoit et al (2018) quanteda: An R package for the quantitative analysis of textual data
}
