
# ANALYSIS VIA LOGISTIC REGRESSION

library(tidyverse)

rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/merged1.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/merged1.RData")


# Constructing Test and Train Matrices ------------------------------------

Xtrain = as.matrix(merged1[1:(0.7*nrow(merged1)), c(4,6)])
Ytrain = as.matrix(merged1[1:(0.7*nrow(merged1)), 7])
Xtest= as.matrix(merged1[(0.7*nrow(merged1)+1):nrow(merged1), c(4,6)])
Ytest = as.matrix(merged1[(0.7*nrow(merged1)+1):nrow(merged1), 7])


# Training ---------------------------------------------------------------

data <- data.frame(Ytrain, Xtrain)
model <- glm(Ytrain ~ ., data, family=binomial(link='logit')) # --> Applying logistic regresion

beta_logistic  = model$coefficients # --> Extracting the betas

# --> Calculating the Etas
eta_log_train = rep(0, nrow(Xtrain))
for (i in 1:nrow(Xtrain)) {
  eta_log_train[i] = round(exp(c(1, Xtrain[i,]) %*% beta_logistic) / (1+exp(c(1,Xtrain[i,]) %*% beta_logistic)),0)
}

# Calculating Training Errors ---------------------------------------------

falsePositivesTrain = 0
falseNegativesTrain = 0

for (i in 1:nrow(Xtrain)) {
  if(eta_log_train[i] == 1 && Ytrain[i] == 0) {
    falsePositivesTrain = falsePositivesTrain + 1
  }
  if(eta_log_train[i] == 0 && Ytrain[i] == 1) {
    falseNegativesTrain = falseNegativesTrain + 1
  }
}

falsePositivesTrain = falsePositivesTrain/nrow(Xtrain) # Calculating a percentage
falseNegativesTrain = falseNegativesTrain/nrow(Xtrain) # Calculating a percentage
accuracyTrain = 1-(falsePositivesTrain + falseNegativesTrain) # Calculating the accuracy


# Plotting ----------------------------------------------------------------

plot(Xtest[,1], Xtest[,2])
points(Xtrain)
x_line = seq(1, 2000)
y_line = (-beta_logistic[1] - beta_logistic[2] * x_line) / beta_logistic[3]
lines(x_line, y_line, col="blue", lwd=2)


# Testing -----------------------------------------------------------------

eta_log_test = rep(0, nrow(Xtest))
for (i in 1:nrow(Xtest)) {
  eta_log_test[i] = round(exp(c(1, Xtest[i,]) %*% beta_logistic) / (1+exp(c(1,Xtest[i,]) %*% beta_logistic)))
}


# Calculating Testing Errors ----------------------------------------------

falsePositivesTest = 0
falseNegativesTest = 0

for (i in 1:nrow(Xtest)) {
  if(eta_log_test[i] == 1 && Ytest[i] == 0) {
    falsePositivesTest = falsePositivesTest + 1
  }
  if(eta_log_test[i] == 0 && Ytest[i] == 1) {
    falseNegativesTest = falseNegativesTest + 1
  }
}

falsePositivesTest = falsePositivesTest/nrow(Xtest) # Calculating a percentage
falseNegativesTest = falseNegativesTest/nrow(Xtest) # Calculating a percentage
accuracyTest = 1-(falsePositivesTest + falseNegativesTest) # Calculating the accuracy

