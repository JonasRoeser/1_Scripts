# GRADIENT BOOSTING

# Setup --------------------------------------------------------------

rm(list = ls())

library(tidyverse)
# install.packages("randomForest")
library(randomForest)
# install.packages("gbm")
library(gbm)
# install.packages("caret")
library(caret)
# install.packages("Ecdat")
library(Ecdat)
# install.packages('e1071', dependencies=TRUE)
library(e1071)

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/DF.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/DF.RData")

set.seed(1)


# Data Preparation ---------------------------------------------------

# Create training and testing data 
DF = as.data.frame(DF[76970:nrow(DF),])


ind = sample(2,nrow(DF), replace=T, prob=c(.7,.3))
train = DF[ind==1,]
test = DF[ind==2,]

# For train function to work it needs outcome vector to be a two level factor
for (i in 1:11) {
  train[,i] = factor(train[,i])
}

# Creating a grid

# The grid allows us to create several different models with various parameter settings.
# Grid features
grid = expand.grid(.n.trees=seq(250,500,by=250),.interaction.depth=seq(1,3,by=1),.shrinkage=seq(.01,.2,by=.09),
                  .n.minobsinnode=seq(1,5,by=2)) 
# Control
control = trainControl(method="CV",number = 10)


# gbm.train = train(Y~.,data=train, method="gbm",trControl=control, tuneGrid=grid)
gbm.train

# Model Training -----------------------------------------------------
 
# for (i in 1:11) {
#   train[,i] = as.numeric(train[,i])
# }
as.character(train$Y)
train$Y = train$Y - 1

gbm.Y <-gbm(Y~.,
            distribution = "bernoulli",
            data = train,
            n.trees = 500,
            interaction.depth = 1,
            shrinkage = .1,
            n.minobsinnode = 1)


summary.gbm(gbm.Y)

# Model Testing ------------------------------------------------------

gbm.Y.test = predict(gbm.Y,
                     newdata = test,
                     type = 'response',
                     n.trees = 250)

# Our test model returns a set of probabilities
# We need to convert this to a simple yes or no and this is done in the code below

gbm.class = ifelse(gbm.Y.test<0.5,0,1)


# We can now look at a table to see how accurate our model is as well as calculate the accuracy
table.class = table(gbm.class,test$Y)
# Accuracy
(accuracy = (table.class[1,1]+table.class[2,2])/(length(gbm.class)))








