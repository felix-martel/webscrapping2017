library(explor)
load("lexpress_table.Rda")

words_hotness <- sapply(words_table,sum)

#words_table <- words_table[,order(words_hotness,decreasing = TRUE)[1:100]]

words_table <- words_table[,colSums(words_table)>0]
words_table <- words_table[rowSums(words_table)>0,]

res.CA <- CA(words_table,graph=F)
h <- HCPC(res.CA)
