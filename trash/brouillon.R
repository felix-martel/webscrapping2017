library(ggplot2)
library(dplyr)

load("trash/clusters.Rda")


words_table_CA <- words_table 
non_zero_words <- names(which(words_table_CA %>% select(-journal,-title,-date) %>% colSums() >0))
words_table_CA <- cbind(words_table_CA[,c(1,2,3)],words_table_CA[,non_zero_words])
non_zero_rows <- which(words_table_CA %>% select(-journal,-title,-date) %>% rowSums() >0)
words_table_CA <- words_table_CA[non_zero_rows,]

#selection des dates avant elections
w<-words_table_CA[which(words_table_CA$date < as.Date("07-05-2017",format="%d-%m-%Y")),]
journaux<-words_table_CA[,'journal']
words_table_CA <- words_table_CA %>% select(-journal,-date,-title)

res.CA <- CA(words_table_CA,graph=F,ncp=20)

words.CA <- as.data.frame(res.CA$col$coord)
words.CA.clust <- kmeans(words.CA,2,iter.max=20,nstart=100)$cluster
words.CA <- words.CA %>% mutate(clust=as.factor(words.CA.clust))

ggplot(words.CA)+aes(x=`Dim 1`,y=`Dim 2`,col=clust)+geom_point()

#plot.HCPC(res.HCPC,invisible=c("row"))
#save(res.HCPC,file="clusters/HCPC.Rda")
