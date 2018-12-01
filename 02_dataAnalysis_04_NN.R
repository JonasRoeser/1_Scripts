# GRADIENT BOOSTING

# Setup --------------------------------------------------------------

rm(list = ls())

library(tidyverse)
# install.packages("nnet")
library(nnet)

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/DF_test.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/DF_test.RData")


# Data Preparation ---------------------------------------------------

# Create training and testing data 
DF_test = df.sample(frac=1)

set.seed(502)
ind = sample(2,nrow(DF_test), replace=T, prob=c(.7,.3))
train = DF_test[ind==1,]
test = DF_test[ind==2,]

neural = function(size, rang, decay, maxit) {
  model <<- nnet(train[,1:4], train[,5], size = size, rang = rang,
              decay = decay, maxit = maxit)
  
  eta_log_train = round(predict(model, train))
  
  falsePositivesTrain = 0
  falseNegativesTrain = 0
  
  for (i in 1:nrow(train)) {
    if(eta_log_train[i] == 1 && train[i,5] == 0) {
      falsePositivesTrain = falsePositivesTrain + 1
    }
    if(eta_log_train[i] == 0 && train[i,5] == 1) {
      falseNegativesTrain = falseNegativesTrain + 1
    }
  }
  
  falsePositivesTrain = falsePositivesTrain/nrow(train) # Calculating a percentage
  falseNegativesTrain = falseNegativesTrain/nrow(train) # Calculating a percentage
  accuracyTrain = 1-(falsePositivesTrain + falseNegativesTrain) # Calculating the accuracy
  
  
  # Testing -----------------------------------------------------------------
  
  eta_log_test = round(predict(model, test))
  
  
  # Calculating Testing Errors ----------------------------------------------
  
  falsePositivesTest = 0
  falseNegativesTest = 0
  
  for (i in 1:nrow(test)) {
    if(eta_log_test[i] == 1 && test[i,5] == 0) {
      falsePositivesTest = falsePositivesTest + 1
    }
    if(eta_log_test[i] == 0 && test[i,5] == 1) {
      falseNegativesTest = falseNegativesTest + 1
    }
  }
  
  falsePositivesTest = falsePositivesTest/nrow(test) # Calculating a percentage
  falseNegativesTest = falseNegativesTest/nrow(test) # Calculating a percentage
  accuracyTest = 1-(falsePositivesTest + falseNegativesTest) # Calculating the accuracy
  
  return(c(accuracyTrain, accuracyTest))
}

neural(4,0.9,0.001,50)

# install.packages("neuralnet")
library(neuralnet)

multineural = function(hidden, learningrate, rep, linear.output) {
  n <- colnames(train)
  f <- as.formula(paste("Y ~", paste(n[!n %in% "Y"], collapse = " + ")))
  model <<- neuralnet(f, data = train, hidden = hidden,
                learningrate = learningrate, rep = rep, linear.output = linear.output)
  
  eta_log_train = round(compute(model, train[,1:4]))
  
  falsePositivesTrain = 0
  falseNegativesTrain = 0
  
  for (i in 1:nrow(train)) {
    if(eta_log_train[i] == 1 && train[i,5] == 0) {
      falsePositivesTrain = falsePositivesTrain + 1
    }
    if(eta_log_train[i] == 0 && train[i,5] == 1) {
      falseNegativesTrain = falseNegativesTrain + 1
    }
  }
  
  falsePositivesTrain = falsePositivesTrain/nrow(train) # Calculating a percentage
  falseNegativesTrain = falseNegativesTrain/nrow(train) # Calculating a percentage
  accuracyTrain = 1-(falsePositivesTrain + falseNegativesTrain) # Calculating the accuracy
  
  
  # Testing -----------------------------------------------------------------
  
  eta_log_test = round(compute(model, test[,1:4]))
  
  
  # Calculating Testing Errors ----------------------------------------------
  
  falsePositivesTest = 0
  falseNegativesTest = 0
  
  for (i in 1:nrow(test)) {
    if(eta_log_test[i] == 1 && test[i,5] == 0) {
      falsePositivesTest = falsePositivesTest + 1
    }
    if(eta_log_test[i] == 0 && test[i,5] == 1) {
      falseNegativesTest = falseNegativesTest + 1
    }
  }
  
  falsePositivesTest = falsePositivesTest/nrow(test) # Calculating a percentage
  falseNegativesTest = falseNegativesTest/nrow(test) # Calculating a percentage
  accuracyTest = 1-(falsePositivesTest + falseNegativesTest) # Calculating the accuracy
  
  return(c(accuracyTrain, accuracyTest))
}

multineural(c(1,1),0.005,2,F)
plot(model)
