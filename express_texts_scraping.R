# Charger
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(lubridate)
library(ggplot2)
library(rvest)
library(readr)
library(tidyverse)

load("lexpress.Rda")

titles_filter <- read_csv("titles_filter.csv",col_names = FALSE)$X1

filter_corpus <- function(h){
  # Convertir le texte en minuscule
  x <- tm_map(h, content_transformer(tolower))
  # Supprimer les mots vides
  x <- tm_map(x, removeWords, stopwords("french"))
  # Supprimer votre propre liste de mots non désirés
  # x <- tm_map(x, removeWords, c("bla"))
  # Supprimer les ponctuations
  x <- tm_map(x, removePunctuation)
  # Supprimer les espaces vides supplémentaires
  x <- tm_map(x, stripWhitespace)
  return(x)
}

words_matrix <- function(h){
  dtm <- TermDocumentMatrix(h)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  res <- data.frame(word = names(v),freq=v)
  res <- res[order(-res$freq),]
  res$word <- factor(res$word,levels=res$word[order(-res$freq)])
  return(res)
}

is_relevant <- function(t,words_filter){
  title_corpus <- Corpus(VectorSource(t))
  title_corpus <- filter_corpus(title_corpus)
  title <- title_corpus$content
  title <- strsplit(title,split=" ",fixed=TRUE)[[1]]
  return(any(words_filter %in% title))
}

relevant_articles <- which(sapply(res$title, function(t) is_relevant(t,titles_filter)))
relevant_words <- read_csv("relevant_words.csv",col_names = FALSE)$X1

scrap_lexpress_text <- function(url,words){
  result <-tryCatch(
    {page <- read_html(url)
    node <- page %>%  html_node("div.article_container")
    text <- node %>% html_nodes("p, h1, h2, h3, h4, h5, h6") %>% html_text %>% stringr::str_c(sep="",collapse="\n")
    text <- filter_corpus(Corpus(VectorSource(text)))
    dtm <- TermDocumentMatrix(text)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    print(url)
    my_d <- data.frame(word = names(v),freq=v)
    return(my_d[words,"freq"])
    },error = function(e) return(rep(NA,length(words))))
  return(result)
}

relevant_articles <- relevant_articles[which(!duplicated(res$title[relevant_articles]))]

words_table <- data.frame(do.call(rbind,lapply(res$link[relevant_articles], FUN = function(url) scrap_lexpress_text(url,relevant_words))))
words_table[is.na(words_table)] <- 0
colnames(words_table) <- relevant_words
rownames(words_table) <- res$title[relevant_articles]

save(words_table,file="lexpress_table.Rda")
