library(rvest)
library(lubridate)

lexpress_scrap <- function(m,d,y){
  
  day = ymd(paste(y,m,d,sep="/"))
  url = paste("https://www.lexpress.fr/archives/",
              gsub("-","/",day),
              "/",
              sep="")
  
  aux <- function(p){
      result <- tryCatch({
        page <- read_html(paste(url,"?p=",p,sep=""))
        nodes <- page %>%  html_nodes(".group")
        topic <- nodes %>% sapply( . %>% html_nodes(".info .topic") %>% html_text )
        title <- nodes %>% sapply( . %>% html_nodes(".title_mu") %>% html_text )
        description <- nodes %>% sapply( . %>% html_nodes(".txt") %>% html_text )
        link <- nodes %>% sapply( . %>% html_nodes("a") %>% html_attr("href") %>% (function(x) paste("http:",x[1],sep="")))
        date <- rep(paste(d,m,y,sep="/"),length(title))
        rbind(cbind(date,topic,title,description,link),aux(p+1))
      }, error = function(e) {data.frame()})
    return(result)
  }
  result <- aux(1)
  print(paste(day,nrow(result)))
  return(result)
}

lexpress = data.frame(date=as.Date(character()),
                 topic= character(),
                 title = character(),
                 description = character(),
                 link = character())



for(d in 1:31){
  lexpress = rbind(lexpress,lexpress_scrap(12,d,2016))
}

for(m in 1:7){
  for(d in 1:31){
    lexpress = rbind(lexpress,lexpress_scrap(m,d,2017))
  }
}

save(lexpress,file="data/lexpress.Rda")






