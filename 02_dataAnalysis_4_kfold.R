# K-FOLD CROSS VALIDATION LOGISTIC REGRESSION

library(tidyverse)

rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/U.RData")


# Because of OneDrive we need to load from two different paths
load("../2_Data/U.RData")

U1 = U[c(5,7,9,11,12)]
U1 = na.omit(U1)

U1 = as.matrix(U1)
# Ytrain = as.matrix(U1[1:(0.7*nrow(U1)), 5])




# accuracy = 


errors = matrix(0, 10, 6 )
colnames(errors) = c("f_pos_train", "f_neg_train", "acc_train", "f_pos_test", "f_neg_test", "acc_test")


set.seed(1)
# Randomly shuffle the data
n = nrow(U1)
# n = 1000
U1 = U1[sample(n),]
# Create 10 equally size folds
folds = cut(seq(1,nrow(U1)),breaks=10,labels=FALSE)
# Perform 10 fold cross validation
for(i in 1:10){
  # Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- U1[testIndexes, ]
  trainData <- U1[-testIndexes, ]
  
  # Splitting X and Y
  Xtrain = trainData[,c(1:4)]
  Ytrain = trainData[,5]
  Xtest = testData[,c(1:4)]
  Ytest = testData[,5]
  
  # Binding X and Y
  data <- data.frame(Ytrain, Xtrain)
  
  # Applying logistic regresion
  model <- glm(Ytrain ~ ., data, family=binomial(link='logit'))
  
  # Extracting the betas
  beta_logistic  = model$coefficients
  
  # Calculating the Etas
  eta_log_train = rep(0, nrow(Xtrain))
  for (j in 1:nrow(Xtrain)) {
    eta_log_train[j] = round(exp(c(1, Xtrain[j,]) %*% beta_logistic) / (1+exp(c(1,Xtrain[j,]) %*% beta_logistic)))
  }
  
  # Calculating Training Errors ---------------------------------------------

  
  for (j in 1:nrow(Xtrain)) {
    if(eta_log_train[j] == 1 && Ytrain[j] == 0) {
      errors[[i,1]] = errors[[i,1]] + 1
    }
    if(eta_log_train[j] == 0 && Ytrain[j] == 1) {
      errors[[i,2]] = errors[[i,2]] + 1
    }
  }
  
  errors[[i,1]] = errors[[i,1]]/nrow(Xtrain) # Calculating a percentage
  errors[[i,2]] = errors[[i,2]]/nrow(Xtrain) # Calculating a percentage
  errors[[i,3]] = 1-(errors[[i,1]] + errors[[i,2]]) # Calculating the accuracy
  
  # Testing -----------------------------------------------------------------
  
  eta_log_test = rep(0, nrow(Xtest))
  for (j in 1:nrow(Xtest)) {
    eta_log_test[j] = round(exp(c(1, Xtest[j,]) %*% beta_logistic) / (1+exp(c(1,Xtest[j,]) %*% beta_logistic)))
  }

  # Calculating Testing Errors ----------------------------------------------

  for (j in 1:nrow(Xtest)) {
    if(eta_log_test[j] == 1 && Ytest[j] == 0) {
      errors[[i,4]] = errors[[i,4]] + 1
    }
    if(eta_log_test[j] == 0 && Ytest[j] == 1) {
      errors[[i,5]] = errors[[i,5]] + 1
    }
  }
  
  errors[[i,4]] = errors[[i,4]]/nrow(Xtest) # Calculating a percentage
  errors[[i,5]] = errors[[i,5]]/nrow(Xtest) # Calculating a percentage
  errors[[i,6]] = 1-(errors[[i,4]] + errors[[i,5]]) # Calculating the accuracy

}

model_acc_log = t(colMeans(errors))

# Plotting ----------------------------------------------------------------

plot(Xtest[,1], Xtest[,2])
points(Xtrain)
x_line = seq(1, 2000)
y_line = (-beta_logistic[1] - beta_logistic[2] * x_line) / beta_logistic[3]
lines(x_line, y_line, col="blue", lwd=2)

