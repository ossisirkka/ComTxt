topic_preprocess_2 <- function(df, tmp_1, select= c("PRON","NOUN","ADJ")){

  tmp_1 <- filter(tmp_1, upos %in% select)
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
