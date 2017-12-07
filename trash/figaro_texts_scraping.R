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

load("lefigaro.Rda")

# Change date
df<-as.character(res_figaro$date)
df2 <-sapply(strsplit(df, "le ", fixed=T), '[', 2)
res_figaro$date<-as.Date(sapply(strsplit(df2, " ", fixed=T), '[', 1), format="%d/%m/%Y")

# select period of time
begin<-"2016-12-01"
end<-"2017-07-31"
res<-subset(res_figaro, res_figaro$date>begin & res_figaro$date < end)
res<-subset(res,!duplicated(res$title))

titles_filter <- read_csv("/Users/q/Desktop/Rprojet/titles_filter.csv",col_names = FALSE)

filter_corpus <- function(h){
  # Convertir le texte en minuscule
  x <- tm_map(h, content_transformer(tolower))
  # Supprimer les mots vides
  x <- tm_map(x, removeWords, stopwords("french"))
  # Supprimer votre propre liste de mots non désirés
  x <- tm_map(x, removeWords, c("après","aujourd'hui","depuis","scan","selon","vidéo","soir","deux","fait","ans","lors","être","plus","dimanche","comme","alors","jeudi","entre","faire","politique"))
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

#relevant_articles <- which(sapply(res$title, function(t) is_relevant(t,titles_filter)))

#load relevant words to study
relevant_words <- read_csv("/Users/q/Desktop/Rprojet/relevant_words.csv",col_names = FALSE)
relevant_words <-relevant_words$X1

#relevant words modified
relevant_words_bis <- read_csv("/Users/q/Desktop/Rprojet/relevant_words_bis.csv",col_names = FALSE)
relevant_words_bis <-relevant_words_bis$X1

scrap_lefigaro_text <- function(url,words){
  result <-tryCatch(
    {page <- read_html(url)
    node <- page %>%  html_node(".fig-content__body")
    chapo <- page %>%  html_node(".fig-content__chapo") %>% html_text
    text <- node %>% html_nodes("p, h2") %>% html_text %>% stringr::str_c(sep="",collapse="\n")
    text <- stringr::str_c(chapo,text,sep="",collapse="\n")
    text <- filter_corpus(Corpus(VectorSource(text)))
    dtm <- TermDocumentMatrix(text)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    print(url)
    my_d <- data.frame(word = names(v),freq=v)
    return(my_d[words,"freq"])
    },error = function(e) {print("error")
    return(rep(NA,length(words)))}
    )
    
  return(result)
}

#relevant_articles <- relevant_articles[which(!duplicated(res$title[relevant_articles]))]
#words_table <- data.frame(do.call(rbind,lapply(res$link[1:10], FUN = function(url) scrap_lefigaro_text(url,relevant_words))))

#scap articles
words_table <- data.frame(do.call(rbind,lapply(res$link, FUN = function(url) scrap_lefigaro_text(paste(url),relevant_words))))

#NA<-0
words_table[is.na(words_table)] <- 0

#colnames & rownames
colnames(words_table) <- relevant_words
rownames(words_table) <- res$title 

#save result
save(words_table,file="lefigaro_table_long.Rda")
words_table<-words_table[relevant_words_bis]
save(words_table,file="lefigaro_table.Rda")
