setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())
suppressMessages(library(tidyverse));suppressMessages(library(GGally))

# loading dataset
data = read.table("data/echocardiogram.txt",h=T,sep=",")
colnames(data)=c("survival","still-alive","age-heart-attack","pericardial-eff",
                 "fractional-short","epss","lvdd","wms","wmi","mult","name","group",
                 "alive-at-1")
View(data)

data = data %>% select(-`still-alive`,-name)  

id = which(data=="?",arr.ind = T)
data[id]=NA

data=na.omit(data)
data = apply(data,1:2,as.numeric)
# proportions 
data %>% select(last_col()) %>% table %>% prop.table %>% barplot(names.arg=c("control","patient"),ylim=c(0,.8))

# correlations by class
data %>% data.frame %>% mutate(alive.at.1=factor(alive.at.1)) %>% ggpairs(aes(color = `alive.at.1`)) + theme_bw()

save.image("Data.RData")
