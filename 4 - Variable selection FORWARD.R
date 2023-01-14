rm(list=ls())
library(rminer);suppressMessages(library(dplyr))
load("Data.RData")

x = data 
x %>% mutate(V58 = factor(V58))

methods = c("5NN","10NN","15NN","LDA","DT","SVM","MULTINOM")
lmeth = length(methods)

names(x)

M = 50               # n of monte carlo inter 
p = .5               # prop of train samples 
n = trunc(p*nrow(x)) # n of train samples 

# Algorithm 

# Partitioning training and test sets for MC
set.seed(1)
id  = matrix(0, nrow = M, ncol = n ) 
for(i in 1:M){id[i,] = sample(1:nrow(x),n)}
Xtr = Xts = Y = list()

for(i in 1:M){
 Xtr[[i]]=x[id[i,],]  # training set
 Xts[[i]]=x[-id[i,],] # testing set
 Y[[i]] = Xts[[i]][,"V58"] # target variable
}

# Forward and model fitting  -----------------------------
head(x)
pool        = lapply(1:lmeth, function(a) 1:57) # 1:57 covariates for each of the 7 methods
regress     = vector("list",lmeth)              # selected vars
continue    = rep(TRUE,lmeth)                   # halt criteria
prsnt_txerr = rep(100,lmeth)                    # current error rate
mod         = vector("list",lmeth)              # stores models for each step
Yhat        = vector("list",lmeth)              # predicted values for each step
classf      = c("knn","knn","knn","lda",        # list of methods used accoring to rminer syntax
                "dt","svm","multinom")

cc = 1 # for progress tracking

t1 = Sys.time()
set.seed(1)
# while any of the continues is true, loop:
while(any(continue)){
 cat("\t\t\t\t\t\t","Iteraction: ",cc,"\n")
 # stores the error rate for each model still in pool
 tx_err = lapply(1:lmeth, function(a) matrix(NA,nrow=M,ncol=length(pool[[a]])))
 pbmthd <- txtProgressBar(min = 0,max = lmeth,style = 3, width = 100,char = "=")
 # loop for each method
 for(ii in 1:lmeth){
  if(continue[ii]==T){
   # MC loop
   cat("\n\t\t\tMC loop for", methods[[ii]], "\n")
   pbmc <- txtProgressBar(min = 0,max = M,style = 3, width = 50,char = "-")
   for(j in 1:M){
    # loop for each var still in pool w/ regress from last step
    for(i in 1:length(pool[[ii]])){
     k = c(regress[[ii]], pool[[ii]][i])
     
     mod[[ii]]    = if(ii<=3){ # k for use on knn only
      fit(V58 ~ ., data = Xtr[[j]][,c("V58", names(x)[k])],
          model = classf[[ii]], task = "c",k=ii*5)
     }else{
      fit(V58 ~ ., data = Xtr[[j]][,c("V58", names(x)[k])],
          model = classf[[ii]], task = "c")
     }
     Yhat[[ii]]    = predict(mod[[ii]], Xts[[j]][,c("V58", names(x)[k])])
     tx_err[[ii]][j,i] = (1 - sum(diag(prop.table(table(Y[[j]], Yhat[[ii]]))))) * 100
    }
    setTxtProgressBar(pbmc, j)
   };close(pbmc)
   # calc min error rate for current step
   temp_txerr = colMeans(tx_err[[ii]])
   id_min  = which.min(temp_txerr)
   cat("  | Last step error rate for", methods[[ii]], ":", prsnt_txerr[[ii]], "\n")
   cat("  | Current step error rate for", methods[[ii]], ":", temp_txerr[id_min], "\n")
   if(prsnt_txerr[[ii]] > temp_txerr[id_min]){
    prsnt_txerr[[ii]] = temp_txerr[id_min]
    regress[[ii]] = c(regress[[ii]], pool[[ii]][id_min])
    pool[[ii]]    = pool[[ii]][-id_min]
   }else{
    continue[[ii]] = FALSE
    cat("  | Done!\n")
   }
   cat("  | A reduced", methods[[ii]], "model includes:\n  |", names(x)[regress[[ii]]],
       "\n  |--------------------------------------------------|\n")
  }else{cat("\n  | Method", methods[[ii]],"done! \n  | Model includes:\n  |", 
            names(x)[regress[[ii]]],
            "\n  | Min error rate:", prsnt_txerr[[ii]],
            "\n  |--------------------------------------------------|\n")}
  setTxtProgressBar(pbmthd, ii)
 };close(pbmthd)
 cc = cc+1
};beepr::beep();t2=Sys.time();print(t2-t1)
#10:07