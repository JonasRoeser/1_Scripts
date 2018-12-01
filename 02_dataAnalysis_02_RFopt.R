# RANDOM FORESTS WITH BAGGING OPTIMIZATION

# Setup-------------------------

library(tidyverse)
library(ggplot2)
library(caret)
library(cowplot)
library(randomForest)
library(pROC)

rm(list = ls())

load("../Roeser, Jonas - 2_Data/DF.RData")
load("../2_Data/DF.RData")


# Preperation ------------

# So we dont get an error due to too big of an vector 
memory.limit(45000)

# Data Formatting
DFopt = DF[1:(0.3*nrow(DF)),]
DFkfold = DF[-(1:(0.3*nrow(DF))),]

DFopt$Y = as.factor(DFopt$Y) #so the random forest function will treat it as a classification problem
DFkfold$Y = as.factor(DFkfold$Y)

# Optimizing   --------------------------------------------------

# Pre-Optimizing Hyperparameters with all features---------------

#Finding the necessary number of trees
temp_model = randomForest(Y ~ ., data= DFopt, ntree=1000)
oob.error.data = data.frame(
  Trees=rep(1:nrow(temp_model$err.rate), times=3),
  Type=rep(c("OOB", "1", "0"), each=nrow(temp_model$err.rate)),
  Error=c(temp_model$err.rate[,"OOB"],
          temp_model$err.rate[,"1"],
          temp_model$err.rate[,"0"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  theme_bw() +
  geom_line(aes(color=Type))
# we can see that the OOB error rate does not decrease significantly after [number of trees]
# it is therefore sufficient to proceed with the standart 500 trees

# Finding the optimal number of variable at each internal nodes
oob_values = vector(length=18)


for(i in 1:18) {
  temp_model = randomForest(Y ~ ., data= DFopt, mtry=i)
  oob_values[i] = temp_model$err.rate[nrow(temp_model$err.rate),1]
}
oob_values
#We can see that mtry = 1 has the lowest OOB error rate and is therefore the optimal 


# Optimizing features with pre-optimized  hyperparameters ----------

load("../Roeser, Jonas - 2_Data/feature_test.RData")

# Optimizing hyperparameters with optimized features --------------





# Plotting ROC curve -------------------

prob_RF_DFopt = as.matrix(predict(model,DFopt, type="prob"))
varImpPlot(model, scale=F)
ROC_Dfopt = roc(Y ~ prob_RF_train[,2],auc = T)
plot(prob_RF_DFopt)


# Kfold --------------------------------

#Creating the errors matrix
errors = matrix(0, 10, 6 )
colnames(errors) = c("f_pos_train", 
                     "f_neg_train", 
                     "acc_train", 
                     "f_pos_test", 
                     "f_neg_test", 
                     "acc_test")

# Create 10 equally size folds
folds = cut(seq(1,nrow(DF)),breaks=10,labels=FALSE)

# Perform 10 fold cross validation
for(i in 1:10){
  # Segement your data by fold using the which() function
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- DFkfold[testIndexes, ]
  trainData <- DFkfold[-testIndexes, ]
  
  # So randomeForest will do a classification, not a regression:
  trainData$Y = as.factor(trainData$Y)
  testData$Y = as.factor(testData$Y)
  
  # Splitting X and Y
  
  Ytrain = as.data.frame(trainData[,ncol(DF)])            
  Ytest = as.data.frame(testData[,ncol(DF)])
  
  # Training model
  model = randomForest(Y ~ ., data=trainData, mtry =1, ntree=500, proximity=TRUE) #######################
  
  # Calculating Training Errors 
  
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
  
  
  # Calculating Testing Errors 
  
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

# Summary 
model_acc_RF = t(colMeans(errors))





# Getting probabilities predictions ----------------------------------






