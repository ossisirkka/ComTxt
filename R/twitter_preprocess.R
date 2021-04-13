library(udpipe)
library(dplyr)
library(purrr)
library(dplyr)

twitter_preprocess <- function(df, ud_lang, stopwords_lang){

  tmp <- data.frame(doc_id = df$status_id, text = df$text, stringsAsFactors = FALSE)
  tmp$text <- tolower(tmp$text)
  tmp$text <- iconv(tmp$text, "UTF-8", sub="")
  tmp$text <- gsub("@\\w*", "", tmp$text)
  tmp$text <- gsub("#\\w*", "", tmp$text)
  tmp$text <- gsub("https://t.co/\\w*", "",  tmp$text)
  tmp$text <- gsub("[[:punct:]]", "", tmp$text)
  tmp$text <- gsub("\\\\s", "",tmp$text)
  tmp$text <- iconv(tmp$text, to = 'UTF-8')

  tmp_1 <- udpipe(x = tmp, object = "spanish", trace = 10)

  tmp_1 <- tmp_1 %>%
      select(doc_id, lemma, upos)

  stop_words <- c(stopwords::stopwords(stopwords_lang, source = "stopwords-iso"),stopwords::stopwords(source = "smart"))

  tmp_1 <- tmp_1[!(tmp_1$lemma %in% stop_words),]
  tmp_1 <- tmp_1[complete.cases(tmp_1),]
  tmp_1 <- tmp_1[tmp_1$upos != "SYM" & tmp_1$upos !="PUNCT",]

  preprocess <- list()
    for(i in 1:length(unique(tmp_1$doc_id))){
      text <-  paste0(tmp_1[tmp_1$doc_id == unique(tmp_1$doc_id)[i],]$lemma, collapse=" ")
      doc <- unique(tmp_1$doc_id)[i]
      preprocess[[i]] <- c(doc, text)
    }
   preprocess_tmp <- as.data.frame(do.call(rbind, preprocess))

   tmp.2 <- df[df$status_id %in% preprocess_tmp$V1, ]
   tmp.2$text <- preprocess_tmp$V2

   return(tmp.2)
}


