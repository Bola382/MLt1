setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())
suppressMessages(library(tidyverse));suppressMessages(library(GGally))

# loading dataset
data = readxl::read_excel("data/dataR2.xlsx")

View(data)

# Data Set Information:
#  
# There are 10 predictors, all quantitative, and a binary dependent variable, 
# indicating the presence or absence of breast cancer. The predictors are 
# anthropometric data and parameters which can be gathered in routine blood 
# analysis. Prediction models based on these predictors, if accurate, 
# can potentially be used as a biomarker of breast cancer.


# Attribute Information:
#  
# Quantitative Attributes:
# Age (years)
# BMI (kg/m2)
# Glucose (mg/dL)
# Insulin (µU/mL)
# HOMA
# Leptin (ng/mL)
# Adiponectin (µg/mL)
# Resistin (ng/mL)
# MCP-1(pg/dL)

# Labels:
# 1=Healthy controls
# 2=Patients

# proportions 
data %>% select(last_col()) %>% table %>% prop.table %>% barplot(names.arg=c("control","patient"),ylim=c(0,.8))

# correlations by class
data %>% ggpairs(aes(color = Classification)) + theme_bw()

save.image("Data.RData")
