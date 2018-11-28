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
load("../2_Data/U.RData")


# Data Preparation ---------------------------------------------------

# Create training and testing data 
DF = as.data.frame(DF[76970:nrow(DF),])

set.seed(502)
ind = sample(2,nrow(DF), replace=T, prob=c(.7,.3))
train = DF[ind==1,]
test = DF[ind==2,]

for (i in 1:11) {
  train[,i] = factor(train[,i])
}


# Creating a grid

# The grid allows us to create several different models with various parameter settings.

grid = expand.grid(.n.trees=seq(200,500,by=200),.interaction.depth=seq(1,3,by=2),.shrinkage=seq(.01,.09,by=.04),
                  .n.minobsinnode=seq(1,5,by=2)) #grid features
control = trainControl(method="CV",number = 10) #control

set.seed(123)

# gbm(formula = formula(data), distribution = "bernoulli",
#     data = list(), weights, var.monotone = NULL, n.trees = 100,
#     interaction.depth = 1, n.minobsinnode = 10, shrinkage = 0.1,
#     bag.fraction = 0.5, train.fraction = 1, cv.folds = 0,
#     keep.data = TRUE, verbose = FALSE, class.stratify.cv = NULL,
#     n.cores = NULL)

gbm.train = train(Y~.,data=train, method="gbm",trControl=control, tuneGrid=grid)
gbm.train

# Model Training -----------------------------------------------------

gbm.Y <-gbm(Y~.,
            distribution = 'bernoulli',
            data = train,
            n.trees = 400,
            interaction.depth = 3,
            shrinkage = .09,
            n.minobsinnode = 1)

summary.gbm(gbm.Y)

# Model Testing ----- 

gbm.Y.test = predict(gbm.Y,
                     newdata = test,
                     type = 'response',
                     n.trees = 400)

# Our test model returns a set of probabilities
# We need to convert this to a simple yes or no and this is done in the code below

gbm.class = ifelse(gbm.Y.test<0.5,0,1)


# We can now look at a table to see how accurate our model is as well as calculate the accuracy
table(gbm.class,test$lfp)
# Accuracy
# (accuracy<-(x+y)/(x+y+...+...))








