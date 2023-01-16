rm(list=ls())
load("Data.RData")
library(ggplot2);library(GGally)
library(dplyr)
rmnames = function(a) {names(a)=NULL;  return(a)}

# describing data
aa = data %>% select(-last_col()) %>% as.matrix
bb = data %>% select(last_col()) %>% unlist %>% rmnames

table(bb)
prop.table(table(bb))

MVN::mvn(aa)

MVN::mvn(aa,mvnTest="mardia")$multivariateNormality
MVN::mvn(aa,mvnTest="hz")$multivariateNormality
MVN::mvn(aa,mvnTest="royston")$multivariateNormality
MVN::mvn(aa,mvnTest="dh")$multivariateNormality
MVN::mvn(aa,mvnTest="energy")$multivariateNormality

vars = c(52, 53,  7, 56, 25, 46, 16, 42, # best vars from variable selection step
         27, 44, 17, 55, 26, 23, 39, 28,
         48, 29, 41, 31,  4, 34, 32)

df = data.frame(aa[,vars[1:5]])
colnames(df)=c("!", "$", "remove","maior seq caixa alta", "hp")

ggpairs(df,aes(color = factor(bb))) + theme_bw()

