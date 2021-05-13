\name{topic_preprocess}
\alias{topic_preprocess}
\title{Preprocess Twitter for topic modelling}
\usage{
topic_preprocess(df, ud_lang, stop_words)
}
\description{
This function retrives lemma version of text data.
}
\arguments{
  \item{df}{twitter data frame}

  \item{ud_lang}{Udpipe langauge}

  \item{stopwords_lang}{stopwords langauges}

}
\value{
  \item{df}{rtweet data frame}

  \item{ud_lang}{"spanish","afrikaans-afribooms",
                          "ancient_greek-perseus", "ancient_greek-proiel", "arabic-padt",
                          "armenian-armtdp", "basque-bdt", "belarusian-hse", "bulgarian-btb",
                          "buryat-bdt", "catalan-ancora", "chinese-gsd", "classical_chinese-kyoto",
                          "coptic-scriptorium", "croatian-set", "czech-cac", "czech-cltt",
                          "czech-fictree", "czech-pdt", "danish-ddt", "dutch-alpino",
                          "dutch-lassysmall", "english-ewt", "english-gum", "english-lines",
                          "english-partut", "estonian-edt", "estonian-ewt", "finnish-ftb",
                          "finnish-tdt",      "french-gsd", "french-partut", "french-sequoia",
                          "french-spoken", "galician-ctg", "galician-treegal", "german-gsd",
                          "gothic-proiel", "greek-gdt", "hebrew-htb", "hindi-hdtb", "hungarian-szeged",
                          "indonesian-gsd", "irish-idt", "italian-isdt", "italian-partut",
                          "italian-postwita", "italian-vit", "japanese-gsd", "kazakh-ktb", "korean-gsd",
                          "korean-kaist", "kurmanji-mg", "latin-ittb", "latin-perseus", "latin-proiel",
                          "latvian-lvtb", "lithuanian-alksnis", "lithuanian-hse", "maltese-mudt",
                          "marathi-ufal",      "north_sami-giella", "norwegian-bokmaal",
                          "norwegian-nynorsk", "norwegian-nynorsklia", "old_church_slavonic-proiel",
                          "old_french-srcmf", "old_russian-torot", "persian-seraji", "polish-lfg",
                          "polish-pdb", "polish-sz", "portuguese-bosque", "portuguese-br",
                          "portuguese-gsd", "romanian-nonstandard", "romanian-rrt", "russian-gsd",
                          "russian-syntagrus", "russian-taiga", "sanskrit-ufal", "serbian-set",
                          "slovak-snk", "slovenian-ssj", "slovenian-sst", "spanish-ancora",
                          "spanish-gsd", "swedish-lines", "swedish-talbanken",      "tamil-ttb",
                          "telugu-mtg", "turkish-imst", "ukrainian-iu", "upper_sorbian-ufal",
                          "urdu-udtb", "uyghur-udt", "vietnamese-vtb", "wolof-wtb"}

  \item{stop_words}{the stopwords data}

}
\examples{
df_ pre <- twitter_preprocess(df_tweets, ud_lang = "spanish", stop_words)

}
\source{
Jan Wijffels (2020) udpipe: Tokenization, Parts of Speech Tagging, Lemmatization and Dependency Parsing with the 'UDPipe' 'NLP' Toolkit
}
