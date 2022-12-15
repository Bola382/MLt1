setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())
library(tidyverse);library(GGally)

# loading dataset
dataTrain = read.table("data/adult.data.txt",h=F,sep=',')
dataTest  = read.table("data/adult.test.txt",h=F,sep=',')

# variable description
# age: continuous.
# workclass: factor
# fnlwgt: continuous.
# education: factor.
# education-num: factor.
# marital-status:factor.
# ocupation: factor.
# relationship: factor.
# race: factor.
# sex: factor.
# capital-gain: continuous.
# capital-loss: continuous.
# hours-per-week: continuous.
# native-country: factor.
# indicator: factor.
cc = c("age", "workclass","fnlwgt","education","education-num","marital-status",
       "ocupation","relationship","race","sex","capital-gain","capital-loss",
       "hours-per-week","native-country","indicator")
colnames(dataTrain) = colnames(dataTest) = cc

rm(cc)

dataTrain = dataTrain %>% tibble
dataTest = dataTest %>% tibble

dataTrain = dataTrain %>% mutate(workclass=factor(workclass),education=factor(education),`education-num`=factor(`education-num`),
                                 `marital-status`=factor(`marital-status`),ocupation=factor(ocupation),
                                 relationship = factor(relationship),race=factor(race),sex=factor(sex),
                                 `native-country` = factor(`native-country`),indicator=factor(indicator))

dataTest = dataTest %>% mutate(workclass=factor(workclass),education=factor(education),`education-num`=factor(`education-num`),
                                 `marital-status`=factor(`marital-status`),ocupation=factor(ocupation),
                                 relationship = factor(relationship),race=factor(race),sex=factor(sex),
                                 `native-country` = factor(`native-country`),indicator=factor(indicator))


# merging train and test datasets for testing different partitions
data = rbind(dataTrain,dataTest)

# describing data
aa = data %>% select(where(is.numeric)) 
psych::describe(aa)

rm(aa)

# proportions for factors
aa = data %>% select(workclass) %>% table %>% prop.table 
aa = merge(aa,select(data,workclass))

aa %>% ggplot(aes(x="workclass", y=aa[,2]*100, fill=workclass)) +
 geom_bar(stat="identity", width=1) +
 coord_polar("y", start=0)


# correlations by rings
data %>% ggpairs(aes(color = rings),cardinality_threshold = 30) + theme_bw()
