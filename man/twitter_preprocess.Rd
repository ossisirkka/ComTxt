\name{twitter_preprocess}
\alias{preprocess textual data}
\title{Preprocess Twitter textual data}
\usage{
twitter_preprocess(df, ud_lang, stopwords_lang)
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

  \item{stopwords_lang}{"af" "ar" "hy" "eu" "bn" "br" "bg" "ca" "zh" "hr" "cs" "da" "nl" "en" "eo" "et" "fi"
 "fr" "gl" "de" "el" "ha" "he" "hi" "hu" "id" "ga" "it" "ja" "ko" "ku" "la" "lt" "lv"
 "ms" "mr" "no" "fa" "pl" "pt" "ro" "ru" "sk" "sl" "so" "st" "es" "sw" "sv" "th" "tl"
"tr" "uk" "ur" "vi" "yo" "zu"}

}
\examples{
df_ pre <- twitter_preprocess(df_tweets, ud_lang = "spanish", stopwords_lang = "es")

}
\source{
Jan Wijffels (2020) udpipe: Tokenization, Parts of Speech Tagging, Lemmatization and Dependency Parsing with the 'UDPipe' 'NLP' Toolkit
}
