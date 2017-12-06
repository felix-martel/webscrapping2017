# Charger
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(lubridate)
library(ggplot2)
library(rvest)

load("lexpress.Rda")

politics_topics = c("Justice","Politique","Elections","FN","Les Républicains","EELV","Vie des Assemblées","LREM","LFI")

politics_articles = res$topic %in% politics_topics

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

# Nuages de mots des titres par mois
for (m in c(12,1:7)){
  titles <- Corpus(VectorSource(res$title[month(dmy(res$date))==m & (res$topic %in% politics_topics)]))
  titles <- filter_corpus(titles)
  d = words_matrix(titles)
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=50, random.order=FALSE, rot.per=0.35,
            colors=brewer.pal(8, "Dark2"))
}

descriptions_corpus <- Corpus(VectorSource(res$description[politics_articles]))
descriptions_corpus = filter_corpus(descriptions_corpus)

# Nuages de mots des descriptions sur toute la période
d = words_matrix(descriptions_corpus)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
