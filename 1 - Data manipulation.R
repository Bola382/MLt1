setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())

# loading dataset
data = read.table("data/spambase.txt",h=F,sep=",")
View(data)

# data set about spam emails
# description given on data/spambase.names
# class var 58: 0 not spam, 1 spam

save.image("Data.RData")
