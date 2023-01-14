rm(list=ls())
load("Data.RData")
library(rminer);suppressMessages(library(dplyr))

rmnames = function(a) {names(a)=NULL;  return(a)}

X = data %>% select(-last_col())
Y = data %>% select(last_col()) %>% unlist %>% rmnames
n  = nrow(X)
C  = length(unique(Y))
m  = 100

k    = n
CV = rep(1:k,each=trunc(n/k),length.out=n)
methods = c("5NN","10NN","15NN","LDA","DT","SVM","MULTINOM")
lmeth = length(methods)

tx_erro = matrix(NA, m, lmeth,
                 dimnames = list(1:m,methods))
# barra de probresso
pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = m, # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=") 

tm = proc.time()[3]
set.seed(1)
for(i in 1:m){
 
 idCV = sample(CV) 
 
 Ypred5NN  = NULL
 Ypred10NN = NULL
 Ypred15NN = NULL
 YpredLDA  = NULL
 YpredDT   = NULL
 YpredSVM  = NULL
 YpredMULTINOM  = NULL
 
 pb2 <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                      max = k, # Maximum value of the progress bar
                      style = 3,    # Progress bar style (also available style = 1 and style = 2)
                      width = 25,   # Progress bar width. Defaults to getOption("width")
                      char = "-") 
 for(g in 1:k){
  
  Xtr = X[idCV!=g,]
  Xts = X[idCV==g,]
  Ytr = Y[idCV!=g]
  Yts = Y[idCV==g]
  
  if(length(unique(Y[idCV!=g])) != C){
   break
   #stop()
  }
  
  dbtreino = cbind.data.frame(Xtr, classe = Ytr)
  dbteste  = cbind.data.frame(Xts, classe = Yts)
  
  mod5NN   = fit(classe ~ ., data = dbtreino,
                 model = "knn", task = "c", k =5)
  mod10NN   = fit(classe ~ ., data = dbtreino,
                  model = "knn", task = "c", k =10)
  mod15NN   = fit(classe ~ ., data = dbtreino,
                  model = "knn", task = "c", k =15)
  modLDA   = fit(classe ~ ., data = dbtreino,
                 model = "lda", task = "c")
  modDT   = fit(classe ~ ., data = dbtreino,
                model = "dt", task = "c")
  modSVM   = fit(classe ~ ., data = dbtreino,
                 model = "svm", task = "c")
  modMULTINOM   = fit(classe ~ ., data = dbtreino,
                      model = "multinom", task = "c")
  
  # 3 - Avaliacao
  Ypred5NN[idCV==g]  = predict(mod5NN, dbteste)
  Ypred10NN[idCV==g] = predict(mod10NN, dbteste)
  Ypred15NN[idCV==g] = predict(mod15NN, dbteste)
  YpredLDA[idCV==g]  = predict(modLDA, dbteste)
  YpredDT[idCV==g]   = predict(modDT, dbteste)
  YpredSVM[idCV==g]  = predict(modSVM, dbteste)
  YpredMULTINOM[idCV==g]  = predict(modMULTINOM, dbteste)
  setTxtProgressBar(pb2, g)
 };close(pb2)
 
 tx_erro[i,"5NN"]  = (1 - sum(diag(prop.table(table(Y, Ypred5NN))))) * 100
 tx_erro[i,"10NN"] = (1 - sum(diag(prop.table(table(Y, Ypred10NN))))) * 100
 tx_erro[i,"15NN"] = (1 - sum(diag(prop.table(table(Y, Ypred15NN))))) * 100
 tx_erro[i,"LDA"]  = (1 - sum(diag(prop.table(table(Y, YpredLDA))))) * 100
 tx_erro[i,"DT"]   = (1 - sum(diag(prop.table(table(Y, YpredDT))))) * 100
 tx_erro[i,"SVM"]  = (1 - sum(diag(prop.table(table(Y, YpredSVM))))) * 100
 tx_erro[i,"MULTINOM"] = (1 - sum(diag(prop.table(table(Y, YpredMULTINOM))))) * 100
 setTxtProgressBar(pb, i)
};close(pb)

print(proc.time()[3] - tm)

# Estimativa de Monte Carlo
colMeans(tx_erro)
apply(tx_erro, 2, FUN = sd)

boxplot(tx_erro)
