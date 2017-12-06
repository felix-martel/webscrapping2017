library(rvest)
library(lubridate)

#scraping
figaro_scrap <- function(x){
  
  number = paste(x)
  url = paste("http://recherche.lefigaro.fr/recherche/pr%C3%A9sidentielles/?page=",
              x,
              sep="")
  
  aux <- function(p){
    print (p)
  result <- tryCatch({
      page <-   read_html(paste(url,p,sep=""))
      #page <- read_html(url)
      print (url)
      nodes <- page %>%  html_nodes(".fig-profil")
      #print (nodes)
      #print (nodes)
      #topic <- nodes %>% sapply( . %>% html_nodes(".fig-tools-rubrique") %>% html_text )
      title <- nodes %>% sapply( . %>% html_nodes(".fig-profil-headline") %>% html_text )
      #title <- nodes %>% sapply( . %>% html_nodes(".href") %>% html_text )
      description <- nodes %>% sapply( . %>% html_nodes(".fig-profil-chapo") %>% html_text )
      link <- nodes %>% sapply( . %>% html_nodes("a") %>% html_attr("href") %>% (function(x) paste("",x[1],sep="")))
      date <- nodes %>% sapply( . %>% html_nodes(".fig-date-pub") %>% html_text )
      rbind(cbind(date,title,description,link),aux(p+1))
    }, error = function(e) {data.frame()})
    return(result)
  }
  result <- aux(1)
  #print(paste(day,nrow(result)))
  return(result)
  }

  #print(paste(day,nrow(result)))

res_figaro = data.frame(date= character(),
                 title = character(),
                 description = character(),
                 link=character()
                 )


res_figaro = data.frame(figaro_scrap(1))
save(res_figaro,file="lefigaro.Rda")

# Change date
df<-as.character(res_figaro$date)
df2 <-sapply(strsplit(df, "le ", fixed=T), '[', 2)
res_figaro$date<-as.Date(sapply(strsplit(df2, " ", fixed=T), '[', 1), format="%d/%m/%Y")


# Write CSV in R
write.csv(res_figaro, file = "lefigaro.csv")

