library(tidyverse)
library(dplyr)

rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/D.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/D.RData")

D = D %>%
  mutate(Y = 1)

D1=D
D1$player1rank[seq(1,22000,2)] = D$player2rank[seq(1,22000,2)]
D1$player2rank[seq(1,22000,2)] = D$player1rank[seq(1,22000,2)]
D1$Y[1:11000] = 0


Xtrain = as.matrix(D1[1:15000, 5:6])
Ytrain = as.matrix(D1[1:15000, 7])
Xtest= as.matrix(D1[15001:22000, 5:6])
Ytest = as.matrix(D1[15001:22000, 7])

data <- data.frame(Ytrain, Xtrain)
model <- glm(Ytrain ~ ., data, family=binomial(link='logit'))

beta_logistic  = model$coefficients

cbind(1,Xtest) %*% beta_logistic
