rm(list=ls())
load("Data.RData")
rmnames = function(a) {names(a)=NULL;  return(a)}

# describing data
aa = data %>% select(-last_col()) %>% as.matrix
bb = data %>% select(last_col()) %>% unlist %>% rmnames

MVN::mvn(aa)

MVN::mvn(aa,mvnTest="mardia")$multivariateNormality
MVN::mvn(aa,mvnTest="hz")$multivariateNormality
MVN::mvn(aa,mvnTest="royston")$multivariateNormality
MVN::mvn(aa,mvnTest="dh")$multivariateNormality
MVN::mvn(aa,mvnTest="energy")$multivariateNormality

vars = c(52, 53,  7, 16, 46, 27, 28, 42, 5, 24, 9, 22, 32, 4, 34)

df = data.frame(aa[,vars[1:5]])
colnames(df)=c("!", "$", "remove","free", "edu")

ggpairs(df,aes(color = factor(bb))) + theme_bw()

