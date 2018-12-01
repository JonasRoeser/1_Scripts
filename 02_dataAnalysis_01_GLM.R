
# LOGISTIC REGRESSION

# Setup -------------------------------------------------------------------

library(tidyverse)
library(caret)
library(pROC)

rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/DF.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/DF.RData")


# Preperation -------------------------------------------------------------

DFopt = DF[1:(0.3*nrow(DF)),]
  
DFkfold = DF[-(1:(0.3*nrow(DF))),]


# Optimisation ------------------------------------------------------------

# Pre-optimising hyperparamters with all features -------------------------

# Creating function that takes feature combination vector as input
logistic = function(comb) {
  DFtrain = DFopt[1:(0.7*nrow(DFopt)),comb]
  Xtrain = as.matrix(DFtrain[,c(1:(ncol(DFtrain)-1))])
  Ytrain <<- as.matrix(DFtrain[,ncol(DFtrain)])
  
  DFtest = DFopt[-(1:(0.7*nrow(DFopt))),comb]
  Xtest = as.matrix(DFtest[,c(1:(ncol(DFtest)-1))])
  Ytest <<- as.matrix(DFtest[,ncol(DFtest)])
  
  # Applying logistic regresion
  model = glm(Y ~ ., data=DFtrain, family=binomial(link='logit'))
  
  # Extracting the betas
  beta_logistic  = model$coefficients
  
  # Calculating training etas & probabilities
  eta_log_train <<- rep(0, nrow(Xtrain))
  prob_log_train <<- rep(0, nrow(Xtrain))
  for (i in 1:nrow(Xtrain)) {
    eta_log_train[i] <<- round(exp(c(1, Xtrain[i,]) %*% beta_logistic) / (1+exp(c(1,Xtrain[i,]) %*% beta_logistic)))
    prob_log_train[i] <<- exp(c(1, Xtrain[i,]) %*% beta_logistic) / (1+exp(c(1,Xtrain[i,]) %*% beta_logistic))
  }
  
  # Calculating training accuracy
  errors = 0
  for (i in 1:nrow(Xtrain)) {
    if(eta_log_train[i] != Ytrain[i]) {
      errors = errors + 1
    }
  }
  accuracy_train = 1-(errors/nrow(Xtrain))
  
  # Calculating testing etas & probabilities
  eta_log_test <<- rep(0, nrow(Xtest))
  prob_log_test <<- rep(0, nrow(Xtest))
  for (i in 1:nrow(Xtest)) {
    eta_log_test[i] <<- round(exp(c(1, Xtest[i,]) %*% beta_logistic) / (1+exp(c(1,Xtest[i,]) %*% beta_logistic)))
    prob_log_test[i] <<- exp(c(1, Xtest[i,]) %*% beta_logistic) / (1+exp(c(1,Xtest[i,]) %*% beta_logistic))
  }
  
  # Calculating testing errors
  errors = 0
  for (i in 1:nrow(Xtest)) {
    if(eta_log_test[i] != Ytest[i]) {
      errors = errors + 1
    }
  }
  accuracy_test = 1-(errors/nrow(Xtest))
  
  return(c(accuracy_train, accuracy_test))
}

# As there are no hyperparameters to optimise for logistic regression, the optimal model with all features is:
logistic(c(1,10))


# Optimising features with pre-optimised hyperparameters ------------------

feature_test = matrix(nrow = 511, ncol = 3)
colnames(feature_test) = c("comb",
                           "accuracy_train",
                           "accuracy_test")

# This for-loop takes quite long to finish
# k = 0
# for(i in 1:9) {
#   for(j in 1:ncol(combn(9,i))) {
#     k = k + 1
#     feature_test[k,1] = paste(rbind(combn(9,i))[,j], collapse = ",")
#     feature_test[k,2:3] = logistic(c(as.double(paste(rbind(combn(9,i))[,j])),10))
#   }
# }
# save(feature_test, file = "../Roeser, Jonas - 2_Data/feature_test.RData")

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/feature_test.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/feature_test.RData")

best_comb = feature_test[which.max(feature_test[,3]),1]
# --> We get the highest testing accuracy training with all feateures, except head to head!


# Optimising hyperparamters with optimised features -----------------------

# Again, there are no hyperparameters to optimise for logistic regression


# Plotting ROC curve ------------------------------------------------------

best_comb

logistic(c(1,2,3,4,5,7,8,9,10))

roc_train = roc(Ytrain ~ prob_log_train,
                auc = T)
plot(roc_train)
roc_test = roc(Ytest ~ prob_log_test,
               auc = T)
plot(roc_test)


# Kfold -------------------------------------------------------------------

logistic_kfold = function(comb) {
  DFtrain = DFkfold[testIndexes,comb]
  Xtrain = as.matrix(DFtrain[,c(1:(ncol(DFtrain)-1))])
  Ytrain = as.matrix(DFtrain[,ncol(DFtrain)])
  
  DFtest = DFkfold[-testIndexes,comb]
  Xtest = as.matrix(DFtest[,c(1:(ncol(DFtest)-1))])
  Ytest = as.matrix(DFtest[,ncol(DFtest)])
  
  # Applying logistic regresion
  model = glm(Y ~ ., data=DFtrain, family=binomial(link='logit'))
  
  # Extracting the betas
  beta_logistic  = model$coefficients
  
  # Calculating training etas & probabilities
  eta_log_train = rep(0, nrow(Xtrain))
  prob_log_train = rep(0, nrow(Xtrain))
  for (i in 1:nrow(Xtrain)) {
    eta_log_train[i] = round(exp(c(1, Xtrain[i,]) %*% beta_logistic) / (1+exp(c(1,Xtrain[i,]) %*% beta_logistic)))
    prob_log_train[i] = exp(c(1, Xtrain[i,]) %*% beta_logistic) / (1+exp(c(1,Xtrain[i,]) %*% beta_logistic))
  }
  
  # Calculating training accuracy
  errors = 0
  for (i in 1:nrow(Xtrain)) {
    if(eta_log_train[i] != Ytrain[i]) {
      errors = errors + 1
    }
  }
  accuracy_train = 1-(errors/nrow(Xtrain))
  
  # Calculating testing etas & probabilities
  eta_log_test = rep(0, nrow(Xtest))
  prob_log_test = rep(0, nrow(Xtest))
  for (i in 1:nrow(Xtest)) {
    eta_log_test[i] = round(exp(c(1, Xtest[i,]) %*% beta_logistic) / (1+exp(c(1,Xtest[i,]) %*% beta_logistic)))
    prob_log_test[i] = exp(c(1, Xtest[i,]) %*% beta_logistic) / (1+exp(c(1,Xtest[i,]) %*% beta_logistic))
  }
  
  # Calculating testing errors
  errors = 0
  for (i in 1:nrow(Xtest)) {
    if(eta_log_test[i] != Ytest[i]) {
      errors = errors + 1
    }
  }
  accuracy_test = 1-(errors/nrow(Xtest))
  
  return(c(accuracy_train, accuracy_test))
}

best_comb

# Create 10 equally sized folds
folds = cut(seq(1,nrow(DFkfold)),breaks=10,labels=FALSE)

# Create kfold matrix
kfold = matrix(nrow = 10, ncol = 2)

# Perform 10 fold cross validation
for(i in 1:10){
  # Segement your data by fold using the which() function
  testIndexes = which(folds==i,arr.ind=TRUE)
  kfold[i,] = logistic_kfold(c(1,2,3,4,5,7,8,9,10))
}

colMeans(kfold)