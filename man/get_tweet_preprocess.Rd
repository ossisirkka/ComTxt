\name{get_tweet_preprocess}
\alias{get_tweet_preprocess}
\title{Preprocess get_tweet function list frame}
\usage{
get_tweet_preprocess(list)
}
\description{
This function preprocess unstructured tweets list data to data frame
}
\arguments{
  \item{list}{list of tweets data from get_tweet function}

}

\examples{
tweets_list <- list()
for(i in seq_len(nrow(dates))){
  tweets_list[[i]] <- get_tweets(
    q = "(#cultura OR #culture) place_country:es",
    n = 500,
    start_time = dates[i,1],
    end_time = dates[i,2],
    token = bearer_token_academic_app)
  Sys.sleep(15)
  print(paste0("Going through row ", i))
}

df_tweets <- get_tweet_preprocess(tweets_list)

}
