\name{clean_location}
\alias{clean_location}
\title{Preprocess profile location data}
\usage{
clean_location(df, location_country = "spain",  multi_name = c("españa","spagna"))
}
\description{
This function preprocess profile location data adds country and city data.
}
\arguments{
  \item{df}{twitter data frame}

  \item{location_country}{country name}

  \item{multi_name}{multiple names of country in different language}

}
\value{
  \item{df}{rtweet data frame}

  \item{location_country}{"spain", "uk", "netherlands", "serbia and montenegro", "croatia", "france", "switzerland", "denmark" ... }

  \item{multi_name}{c("españa","spagna"),  c(dänemark), c("republic of serbia", "srbija"), c("republic of croatia"),  c("schweiz", "suisse", "suiza"), c("united kingdom", "britain") ... }
}
\examples{
df_ pre <- twitter_preprocess(df_tweets, ud_lang = "spanish", stopwords_lang = "es")
df_ pre <- clean_location(df_ pre, location_country = "spain",  multi_name = c("españa","spagna"))
}

