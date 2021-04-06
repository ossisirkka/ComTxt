# Topic modelling

# This is an example function named 'get_topics'
# which looks for topics in your original text.

library(mallet)
library(stopwords)
twitter_topic <- function(df, language, n_topic){
##load  data
    df$status_id <- as.character(as.factor(df$status_id))
    df <- df[, c("status_id", "text")]
    colnames(df) <- c("doc.id", "text")

    library(stringr)
    for(i in 1:nrow(df)){
        df$text[[i]] <- paste(str_extract_all(df$text[[i]], '\\w{4,}')[[1]], collapse=' ')
    }


    stop.tmp <- stopwords::data_stopwords_smart
    stopwords_text <- stopwords::stopwords(language, source = "stopwords-iso")
    stopwords_text <- c(stop.tmp, stopwords_text)
    write.table(stopwords_text, file="stopwords_es.txt", fileEncoding="UTF-8", row.names = FALSE, col.names =  FALSE,quote = FALSE)


    ##mallet analysis
    ##import as mallet format
    mallet.instances <- mallet.import(df$doc.id, df$text, paste0("stopwords_", language, ".txt"), token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

    ##mallet analysis
     model <- MalletLDA(num.topics = as.numeric(n_topic))
     model$model$setRandomSeed(12345L)
     model$loadDocuments(mallet.instances)

     ## Optimize hyperparameters every 20 iterations, after 50 burn-in iteration
     model$setAlphaOptimization(2000, 4000)
     model$train(2000)
     model$maximize(30)
     mallet_df <- model
}


