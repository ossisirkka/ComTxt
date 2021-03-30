\name{topic_network}
\alias{Mallet Topic Modelling}
\title{The network plot of topic model}
\usage{
topic_network(mallet_df, n_nodes, igraph_semantic_label)
}
\description{
This function display semantic network of topic model.
}
\arguments{
  \item{mallet_df}{The model trained by get_topic function}

  \item{n_nodes}{The number of nodes/words. default 50}

  \item{igraph_semantic_label}{The size of vertex labels. default 0.01}
}

\examples{


topics <- get_topic(basic_tweets, language = "es", n_topic = 7)
topic_network(topics, n_nodes = 50, igraph_semantic_label= 0.007)
}
\source{
Csardi G, Nepusz T (2006). The igraph software package for complex network research.

David Mimno (2013) mallet: A wrapper around the Java machine learning tool MALLET
}
