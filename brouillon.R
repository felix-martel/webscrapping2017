library(ggplot2)
library(dplyr)

load("trash/clusters.Rda")


repart <- res.HCPC$data.clust %>% 
  select(journal,clust) %>% 
  group_by(journal,clust) %>% 
  summarise(prop = n())

poids_journaux <- repart %>% 
  select(journal,prop) %>% 
  group_by(journal) %>% 
  summarise(poids = sum(prop))

taille_clusters <- repart %>% 
  select(clust,prop) %>% 
  group_by(clust) %>% 
  summarise(taille = sum(prop)) %>% 
  mutate(taille = taille/sum(taille))

repart <- repart %>% 
  left_join(poids_journaux,by = "journal") %>% 
  mutate(prop = prop / poids) %>% 
  select(clust,journal,prop)

ggplot(repart)+
  aes(x=clust,y=prop,fill=journal)+
  geom_col()+xlab("cluster")+
  ylab("répartition")

ggplot(taille_clusters)+
  aes(x=clust,y=taille)+
  geom_col()+xlab("cluster")+
  ylab("taille")
