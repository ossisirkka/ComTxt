\name{topic_wordplot}
\alias{topic word plot}
\title{The word term plot for topics}
\usage{
topic_wordplot(mallet_df, k, num_words = 10)
}
\description{
This function display word term plot of topics.
}
\arguments{
  \item{mallet_df}{The model trained by get_topic function}

  \item{k}{The numbers topics of model}

  \item{num_words}{The number top terms. default 10}

}

\examples{

df_ pre <- twitter_preprocess(df_tweets, ud_lang = "spanish", stopwords_lang = "es")
df_topics <- twitter_topic(df_pre, language = "es", n_topic = 7)
topic_wordplot(df_topics, k, num_words = 10)

}
\source{
David Mimno (2013) mallet: A wrapper around the Java machine learning tool MALLET
}
