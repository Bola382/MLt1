load("Data.RData")

# describing data
aa = data %>% select(-last_col()) 

MVN::mvn(aa)

MVN::mvn(aa,mvnTest="mardia")$multivariateNormality
MVN::mvn(aa,mvnTest="hz")$multivariateNormality
MVN::mvn(aa,mvnTest="royston")$multivariateNormality
MVN::mvn(aa,mvnTest="dh")$multivariateNormality
MVN::mvn(aa,mvnTest="energy")$multivariateNormality

# pca
pcaa = prcomp(aa,center = T, scale=T)

aa = cbind(pcaa$x,select(data,last_col()))

aa %>% ggpairs(aes(color = Classification)) + theme_bw()
