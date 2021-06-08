\name{geo_map_1}
\alias{geo_map_1}
\title{Drawing twitter map}
\usage{
geo_map_1(country, df_2019, df_2020)
}
\description{
This function draw location map.
}
\arguments{
  \item{country}{name of country}

  \item{df_2019}{Data frame for 2019}

  \item{df_2020}{Data frame for 2020}
}
\value{
  \item{country}{"spain", "uk", "netherlands", "serbia and montenegro", "croatia", "france", "switzerland", "denmark" ... }

}
\examples{

##### Example ####
tweets_map("spain", # Country
           df_2019, # Data frame for 2019
           df_2020) # Data frame for 2020

}

