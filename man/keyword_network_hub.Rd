\name{keyword_network_hub}
\alias{keywords Semantic network hub}
\title{Keywords Semantic network hub}
\usage{
keyword_network_hub(df, hub, n_top)
}
\description{
This function draws zoomed in semantic network around hub.
}
\arguments{
  \item{df}{The preprocessed twitter data}

  \item{hub}{zoomed in word}

  \item{n_top}{number of nodes in the plot}
}

\examples{

df_pre <- twitter_preprocess(twitter_df, ud_lang = "spanish", stopwords_lang = "es")
keyword_network_hub(df, hub = "art", n_top = 40)
}
\source{
Benoit et al (2018) quanteda: An R package for the quantitative analysis of textual data
}
