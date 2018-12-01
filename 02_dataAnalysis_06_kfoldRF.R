# K-FOLD CROSS VALIDATION RANDOM FOREST WITH BAGGING

library(tidyverse)
library(dplyr)

rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/DF.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/DF.RData")

set.seed(1)


# Data Preperation ---------------------------------------------------

# We cannot take the whole dataset because of computational restrictions (so we dont get 70 gb)
DF= sample_n(as.data.frame(DF), 3000)

#Creating the errors matrix
errors = matrix(0, 10, 6 )
colnames(errors) = c("f_pos_train", 
                     "f_neg_train", 
                     "acc_train", 
                     "f_pos_test", 
                     "f_neg_test", 
                     "acc_test")

# Selecting amount of data to run
n = nrow(DF)
# n = 1000
DF = DF[sample(n),]

# Create 10 equally size folds
folds = cut(seq(1,nrow(DF)),breaks=10,labels=FALSE)

# Perform 10 fold cross validation
for(i in 1:10){
  # Segement your data by fold using the which() function
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- DF[testIndexes, ]
  trainData <- DF[-testIndexes, ]
  
  # So randomeForest will do a classification, not a regression:
  trainData$Y = as.factor(trainData$Y)
  testData$Y = as.factor(testData$Y)
  
  # Splitting X and Y

  Ytrain = as.data.frame(trainData[,ncol(DF)])            
  Ytest = as.data.frame(testData[,ncol(DF)])
  
  # Training model
  model = randomForest(Y ~ ., data=trainData, mtry =1, ntree=500, proximity=TRUE) 

  # Calculating Training Errors ---------------------------------------------

  # Calculating the training eta
  eta_RF_train = as.data.frame(predict(model,type="response",
                                norm.votes=TRUE, predict.all=FALSE, proximity=FALSE, nodes=FALSE))

  # Calculating training error
  for (j in 1:nrow(trainData)) {
    if(eta_RF_train[j,1] == 1 && Ytrain[j,1] == 0) {
      errors[[i,1]] = errors[[i,1]] + 1
    }
    if(eta_RF_train[j,1] == 0 && Ytrain[j,1] == 1) {
      errors[[i,2]] = errors[[i,2]] + 1
    }
  }
  
  # Calculating error percentages
  errors[[i,1]] = errors[[i,1]]/nrow(eta_RF_train)
  errors[[i,2]] = errors[[i,2]]/nrow(eta_RF_train)
  
  # Calculating the accuracy
  errors[[i,3]] = 1-(errors[[i,1]] + errors[[i,2]]) 
 

  # Calculating Testing Errors ---------------------------------------
  
  # Calculating the testing eta
  eta_RF_test = as.data.frame(predict(model,testData, type="response",
                                     norm.votes=TRUE, predict.all=FALSE, proximity=FALSE, nodes=FALSE))

  # Calculating testing errors
  for (j in 1:nrow(testData)) {
    if(eta_RF_test[j,1] == 1 && Ytest[j,1] == 0) {
      errors[[i,4]] = errors[[i,4]] + 1
    }
    if(eta_RF_test[j,1] == 0 && Ytest[j,1] == 1) {
      errors[[i,5]] = errors[[i,5]] + 1
    }
  }
  # Calculating error percentages
  errors[[i,4]] = errors[[i,4]]/nrow(eta_RF_test)
  errors[[i,5]] = errors[[i,5]]/nrow(eta_RF_test)
  
  # Calculating the accuracy
  errors[[i,6]] = 1-(errors[[i,4]] + errors[[i,5]])
  
}

# Summary ------------------------------------------------------------
model_acc_RF = t(colMeans(errors))


