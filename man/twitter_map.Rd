\name{twitter_map}
\alias{twitter_map}
\title{Drawing twitter map}
\usage{
twitter_map(df, location_country)
}
\description{
This function draw location map.
}
\arguments{
  \item{df}{twitter data frame}

  \item{location_country}{country name}


}
\value{
  \item{df}{rtweet data frame}

 \item{location_country}{"spain", "uk", "netherlands", "serbia and montenegro", "croatia", "france", "switzerland", "denmark" ... }

}
\examples{

twitter_map(df_pre, location_country = "spain")

}

