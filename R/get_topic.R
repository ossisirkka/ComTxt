# Topic modelling
#
# This is an example function named 'get_topics'
# which looks for topics in your original text.

get_topic <- function(df, language, mt_n){
##load  data
    df_attach <- df$status_id <- as.character(as.factor(df$status_id))
    df <- df[, c("status_id", "text")]
    colnames(df) <- c("doc.id", "text")

    ##mallet analysis
    ##import as mallet format
    mallet.instances <- mallet.import(df_attach$doc.id, df_attach$text, paste0("stopwords_", language, ".txt"), token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

    ##mallet analysis
     model <- MalletLDA(num.topics = as.numeric(mt_n))
     model$model$setRandomSeed(12345L)
     model$loadDocuments(mallet.instances)

     ## Optimize hyperparameters every 20 iterations, after 50 burn-in iteration
     model$setAlphaOptimization(2000, 4000)
     model$train(2000)
     model$maximize(30)
     mallet_df <- model
}



