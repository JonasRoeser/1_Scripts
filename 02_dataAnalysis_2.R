library(tidyverse)
library(dplyr)

rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/merged1.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/merged1.RData")

# Training ---------------------------------------------------------------

Xtrain = as.matrix(merged1[1:(0.7*nrow(merged1)), c(4,6)])
Ytrain = as.matrix(merged1[1:(0.7*nrow(merged1)), 7])
Xtest= as.matrix(merged1[(0.7*nrow(merged1)+1):nrow(merged1), c(4,6)])
Ytest = as.matrix(merged1[(0.7*nrow(merged1)+1):nrow(merged1), 7])

data <- data.frame(Ytrain, Xtrain)
model <- glm(Ytrain ~ ., data, family=binomial(link='logit'))

beta_logistic  = model$coefficients

model$fitted.values
cbind(1,Xtest) %*% beta_logistic

eta_log = rep(0, nrow(Xtrain))
for (i in 1:nrow(Xtrain)) {
  eta_log[i] = round(exp(c(1, Xtrain[i,]) %*% beta_logistic) / (1+exp(c(1,Xtrain[i,]) %*% beta_logistic)),0)
}

# length(which(eta_log == Ytrain))
# length(which(eta_log != Ytrain))
falsePositives = 0
falseNegatives = 0
# accuracy = 0

for (i in 1:nrow(Xtrain)) {
  if(eta_log[i] == 1 && Ytrain[i] == 0) {
    falsePositives = falsePositives + 1
  }
  if(eta_log[i] == 0 && Ytrain[i] == 1) {
    falseNegatives = falseNegatives + 1
  }
}
falsePositives = falsePositives/nrow(Xtrain)
falseNegatives = falseNegatives/nrow(Xtrain)
accuracy = 1-(falsePositives + falseNegatives)

# errors(Xtrain, Ytrain) # rausnehmen, da Funktion gelöscht

# Plotting ----------------------------------------------------------------

plot(Xtest[,1], Xtest[,2])
points(Xtrain)
x_line = seq(1, 2000)
y_line = (-beta_logistic[1] - beta_logistic[2] * x_line) / beta_logistic[3]
lines(x_line, y_line, col="blue", lwd=2)


# Testing -----------------------------------------------------------------

# eta_log = rep(0, nrow(Xtrain))
# for (i in 1:nrow(Xtrain)) {
#   eta_log[i] = exp(c(1, Xtrain[i,]) %*% beta_logistic) / (1+exp(c(1,Xtrain[i,]) %*% beta_logistic))
# }



