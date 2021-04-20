##set the number of topics
## call load packages
library(SparseM)
library(lava)
library(RTextTools)
library(tm)
library(slam)
library(topicmodels)
library(Rmpfr)
## make a plot with
library(ggplot2)

library(grid)
library(gridExtra)
## with the smallest document division df.topic
## transfer to matrix
#tmp <- tempfile()
#writeLines(stopwords.text, tmp)

topic_number <- function(df){
  set.seed(123)
  matrix <- DocumentTermMatrix(Corpus(VectorSource(df$text)))

## Find the best K Selection by Harmonic Mean. ; The harmonic mean function:
harmonicMean <- function(logLikelihoods, precision = 2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}

##  We will use a sequence of numbers from 2 to 100, stepped by one. Using the lapply function, we run the LDA function using all the values of k. To see how much time is needed to run the process on your system, use the system.time function.

seqk <- seq(2, 40, 1)

burnin <- 4000 #1000
iter <- 2000 #1000
keep <- 100 #50
system.time(fitted_many <- lapply(seqk,
                                  function(k) topicmodels::LDA(matrix,
                                                               k = k,
                                                               method = "Gibbs",
                                                               control = list(burnin = burnin,
                                                                              iter = iter,
                                                                              keep = keep) )))

# extract logliks from each topic
logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])

# compute harmonic means
hm_total <- sapply(logLiks_many, function(h) harmonicMean(h))
seqk[which.max(hm_total)]
k <- seqk[which.max(hm_total)]

#plot(seqk, hm_total, type = "l")

a <- ggplot(data.frame(seqk, hm_total), aes(x = seqk, y = hm_total)) + geom_path(lwd = 1.5) +
  theme(text = element_text(family = NULL),
        axis.title.y = element_text(vjust = 1, size = 16),
        axis.title.x = element_text(vjust = -.5, size = 16),
        axis.text = element_text(size = 16),
        plot.title = element_text(size = 20)) +
  xlab('Number of Topics') +
  ylab('Harmonic Mean') +
  #ylim(c(-10000000,-7500000)) +
  #annotate("text", label = paste("The optimal number of topics is", seqk[which.max(hm_total)])) +
  ggtitle(expression(atop("Best Topic number by harmonicMean", atop(italic("How many distinct topics?"), "")))) +
  theme(axis.ticks = element_line(size = 1))+
  scale_x_continuous(breaks = seq(1, 50,by = 1))+
  theme_minimal()

## by perperxity
#dtm <- DocumentTermMatrix(Corpus(VectorSource(df$text)))

#Filtering empty rows of the matrix (DTM) - important for performance
#rowTotals <- apply(dtm, 1, sum)
#dtm_filter <- dtm[rowTotals > 0,]

#Model optimization - calculating perplexity for different values of k
#set.seed(123)
#train = sample(rownames(dtm_filter), nrow(dtm_filter) * .75)
#dtm_train = dtm_filter[rownames(dtm_filter) %in% train, ]
#dtm_test = dtm_filter[!rownames(dtm_filter) %in% train, ]

#perplexity = data.frame(k = 2:40, p=NA) #calculating perplexity for k 5:20
#for (k in perplexity$k) {
#  message("k=", k)
#  m = LDA(dtm_train, method = "Gibbs", k = k,  control = list(alpha = 5/k))
#  perplexity$p[perplexity$k==k] = perplexity(m, dtm_test)
#}
#perplexity

#b <- ggplot(perplexity, aes(x = k, y = p)) +
#  geom_point() +
#  geom_line(group = 1)+
#  ggtitle("Best topic number by perplexity score") + theme_minimal() +
#  scale_x_continuous(breaks = seq(1,40,1)) + ylab("perplexity")


#create DTM
dtm <- CreateDtm(df$text,
                 doc_names = df$status_id,
                 ngram_window = c(1, 2))
#explore the basic frequency
tf <- TermDocFreq(dtm = dtm)
original_tf <- tf %>% select(term, term_freq,doc_freq)
rownames(original_tf) <- 1:nrow(original_tf)
# Eliminate words appearing less than 2 times or in more than half of the
# documents
vocabulary <- tf$term[ tf$term_freq > 1 & tf$doc_freq < nrow(dtm) / 2 ]
dtm = dtm


k_list <- seq(1, 40, by = 1)
model_dir <- paste0("models_", digest::digest(vocabulary, algo = "sha1"))
if (!dir.exists(model_dir)) dir.create(model_dir)
model_list <- TmParallelApply(X = k_list, FUN = function(k){
  filename = file.path(model_dir, paste0(k, "_topics.rda"))

  if (!file.exists(filename)) {
    m <- FitLdaModel(dtm = dtm, k = k, iterations = iter)
    m$k <- k
    m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
    save(m, file = filename)
  } else {
    load(filename)
  }

  m
}, export=c("dtm", "model_dir")) # export only needed for Windows machines
#model tuning
#choosing the best model
coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)),
                            coherence = sapply(model_list, function(x) mean(x$coherence)),
                            stringsAsFactors = FALSE)
c <- ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic number by Coherence Score") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,40,1)) + ylab("Coherence")
pushViewport(viewport(layout = grid.layout(2, 2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(a, vp = vplayout(1, 1:2))  # key is to define vplayout
#print(b, vp = vplayout(2, 1:2))
print(c, vp = vplayout(2, 1:2))
}


