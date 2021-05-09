\name{geo_profile}
\alias{geo_profile}
\title{Preprocess profile location data}
\usage{
geo_profile(df, location_country,  multi_name )
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
df_tweets <- get_tweet_preprocess(tweets_list, location_country = "spain")
df_pre <- geo_profile(df_tweets, location_country = "spain",  multi_name = c("españa","spagna"))
}

