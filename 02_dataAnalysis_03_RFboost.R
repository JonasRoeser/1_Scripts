# GRADIENT BOOSTING ??RANDOMFOREST

# Setup --------------------------------------------------------------

rm(list = ls())

library(tidyverse)
library(plyr)
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
# Hier fehlt eins hab aber vergessen wie es heisst

library(mlbench)
# install.packages("MLmetrics")
library(MLmetrics)

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/DF.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/DF.RData")

# For reproducibility
set.seed(1)


# Data Preparation ---------------------------------------------------

# Create training and testing data 
ind = sample(2,nrow(DF), replace=T, prob=c(.7,.3))
train = DF[ind==1,]
test = DF[ind==2,]

# Take subset of data due to speed issues
train = train[1:3000,]
test = test[1:3000,]

# Creating a Grid ----------------------------------------------------

# The grid allows us to create several different models with various parameter settings.
# Grid features
grid = expand.grid(.n.trees           = seq(100,400, by=25),
                   .interaction.depth = seq(1,5, by=1),
                   .shrinkage         = seq(.01,.1, by=.01),
                   .n.minobsinnode    = seq(1,10, by=1)
)
# Control
control = trainControl(method="CV",number = 5)

# Train() needs the ouput to be a factor of two levels
train[,ncol(train)] = as.factor(train[,ncol(train)])
test[,ncol(train)] = as.factor(test[,ncol(train)])


# gbm.train = train(Y~.,
#                   data      = train,
#                   method    = "gbm",
#                   trControl = control,
#                   tuneGrid  = grid
#                   )
# # The parameters change with the amount of data we train with:
# gbm.train
# # Accuracy was used to select the optimal model using the largest value.
# # The final values used for the model were
# # n.trees           = 225
# # interaction.depth = 5
# # shrinkage         = 0.05
# # n.minobsinnode    = 3


# Gradient Boosting --------------------------------------------------

train[,ncol(train)] = as.numeric(levels(train[,ncol(train)]))[train[,ncol(train)]]
test[,ncol(test)] = as.numeric(levels(test[,ncol(test)]))[test[,ncol(test)]]


gbm.model = gbm(Y~.,
                data              = train, 
                interaction.depth = 5,
                n.minobsinnode    = 3,
                shrinkage         = 0.05, 
                distribution      = 'bernoulli', 
                cv.folds          = 5,
                n.trees           = 225
)
best.iter = gbm.perf(gbm.model, method="cv")
best.iter
summary(gbm.model)

# Plotting I ---------------------------------------------------------

# Need to backscale to original df for visually understandable results
# Plotting the marginal effect of selected variables by "integrating" 
# out the other variables
# plot.gbm(gbm.model, 1, best.iter)
# plot.gbm(gbm.model, 2, best.iter)
# plot.gbm(gbm.model, 3, best.iter)
# plot.gbm(gbm.model, 4, best.iter)
# plot.gbm(gbm.model, 5, best.iter)
# plot.gbm(gbm.model, 6, best.iter)
# plot.gbm(gbm.model, 7, best.iter)
# plot.gbm(gbm.model, 8, best.iter)
# plot.gbm(gbm.model, 9, best.iter)

# ??? ------

# ???
fitControl = trainControl(method="cv", number=5, returnResamp = "all")

# Model 2 -----

train[,ncol(train)] = as.factor(train[,ncol(train)])
test[,ncol(train)] = as.factor(test[,ncol(train)])

model2 = train(Y~., 
               data = train, # test or train?
               method = "gbm",
               distribution = "bernoulli", 
               trControl = fitControl, 
               verbose = F, 
               tuneGrid = data.frame(.n.trees=best.iter, 
                                     .shrinkage=0.04, 
                                     .interaction.depth=1, 
                                     .n.minobsinnode=5))

model2
confusionMatrix(model2)
getTrainPerf(model2)

mPred = predict(model2, test, na.action = na.pass) # test or train?
postResample(mPred, test$Y)
confusionMatrix(mPred, test$Y)

# # ???? ----
# 
# mResults = predict(model2, test, na.action = na.pass, type = "prob")
# mResults$obs = test$Y
# head(mResults)
# 
# mnLogLoss(mResults, lev = levels(mResults$obs))
# 
# mResults$pred = predict(model2, test, na.action = na.pass)
# multiClassSummary(mResults, lev = levels(mResults$obs))
# 
# # ???? ----
# 
# evalResults <- data.frame(Class = test$Y)
# evalResults$GBM <- predict(model2, test, na.action = na.pass, type = "prob")[,1]
# head(evalResults)
# 
# trellis.par.set(caretTheme())
# liftData <- lift(Class ~ GBM, data = evalResults)
# plot(liftData, values = 60, auto.key = list(columns = 1,
#                                             lines = TRUE,
#                                             points = FALSE))
