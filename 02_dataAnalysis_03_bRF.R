
# GRADIENT BOOSTED RANDOM FOREST

# Setup -------------------------

library(tidyverse)
library(caret)
library(pROC)
library(plyr)
library(randomForest)
library(gbm)
library(Ecdat)
# install.packages('e1071', dependencies=TRUE)
library(e1071)
library(mlbench)
library(MLmetrics)
# Hier fehlt eins hab aber vergessen wie es heisst

rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/DF.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/DF.RData")


# Preperation ------------

# For reproducibility
set.seed(1)

# Data Formatting
DFopt = DF[1:(0.3*nrow(DF)),]
DFkfold = DF[-(1:(0.3*nrow(DF))),]

# Train() needs the ouput to be a factor of two levels
DFopt[,ncol(DFopt)] = as.factor(DFopt[,ncol(DFopt)])
DFkfold[,ncol(DFkfold)] = as.factor(DFkfold[,ncol(DFkfold)])


# Optimisation --------------------------------------------------

# Pre-optimising hyperparameters with all features ---------------

# The grid allows us to create several different models with various parameter settings.
# Grid features
grid = expand.grid(.n.trees = c(500,1000,2000),
                   .interaction.depth = 5,
                   .shrinkage = c(0.05,0.1),
                   .n.minobsinnode = seq(2,5, by=1)
)

# We are using 5-fold cross-validation for paramter tuning
control = trainControl(method = "cv",
                       number = 5)

temp_model = train(Y ~ .,
                  data      = DFopt,
                  method    = "gbm",
                  trControl = control,
                  tuneGrid  = grid
)

# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were
# n.trees           = 500
# interaction.depth = 5
# shrinkage         = 0.05
# n.minobsinnode    = 2


# Optimising features with pre-optimized hyperparameters ----------

# Reading the feature_test created with logistic regression
load("../Roeser, Jonas - 2_Data/feature_test.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/feature_test.RData")

# Creating a matrix for testing the 10 best feature combinations of logistic regression
feature_test_brf = matrix(nrow = 10, ncol = 2)
colnames(feature_test_brf) = c("comb",
                               "boosted_accuracy")
feature_test_brf[,1] = feature_test[order(feature_test[,3], decreasing=T)[1:10],1]

# Creating function that takes feature combination vector as input
boosted = function(comb) {
  DFopt = DFopt[,comb]
  
  grid = expand.grid(.n.trees = 500,
                     .interaction.depth = 5,
                     .shrinkage = 0.05,
                     .n.minobsinnode = 2
  )
  
  # We are using 5-fold cross-validation for paramter tuning
  control = trainControl(method = "cv",
                         number = 5)
  
  temp_model = train(Y ~ .,
                     data = DFopt,
                     method = "gbm",
                     tuneGrid = grid,
                     trControl = control
  )
  
  return(temp_model$results$Accuracy)
}

for(i in 1:10) {
  feature_test_brf[i,2] = boosted(c(unlist(lapply(strsplit(feature_test_brf[i,1], split=","), as.numeric)),10))
}

best_comb = feature_test_brf[which.max(feature_test_brf[,2]),1]
# --> We get the highest testing accuracy when training with all feateures, except fatigue!

# Optimising hyperparameters with optimized features --------------

# Now we use the optimal feature combination from above
best_comb

# The grid allows us to create several different models with various parameter settings.
# Grid features
grid = expand.grid(.n.trees = c(400,600),
                   .interaction.depth = 5,
                   .shrinkage = c(0.03,0.06),
                   .n.minobsinnode = c(1.75,2.25)
)

# We are using 5-fold cross-validation for paramter tuning
control = trainControl(method = "cv",
                       number = 5)

model = train(Y ~ .,
              data = DFopt[,c(1,2,3,4,5,6,7,9,10)],
              method = "gbm",
              tuneGrid = grid,
              trControl = control
)

# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were
# n.trees           = 400
# interaction.depth = 5
# shrinkage         = 0.03
# n.minobsinnode    = 2.25


# Plotting ROC curve -------------------

prob_bRF_DFopt = as.matrix(predict(model,DFopt[,c(1,2,3,4,5,6,7,9,10)], type="prob"))
varImpPlot(model, scale=F)
ROC_Dfopt = roc(Y ~ prob_NN_DFopt[,2],auc = T)
plot(prob_RF_DFopt)


# Kfold --------------------------------

# Perform 10 fold cross validation
boosted_kfold = function (data,comb) {
  data = data[,comb]
  
  grid = expand.grid(.n.trees = 400,
                     .interaction.depth = 5,
                     .shrinkage = 0.03,
                     .n.minobsinnode = 2.25
  )
  
  control = trainControl(method = "cv",
                         number = 10)
  
  model = train(Y ~ .,
                data = data,
                method = "gbm",
                tuneGrid = grid,
                trControl = control
  )
  
  return(model$results$Accuracy)
}

# Because we are unable to handle very large amounts of data, we split DFkfold up into 3 subsets
DFkfold1 = DFkfold[1:(nrow(DFkfold)/3),]
DFkfold2 = DFkfold[(nrow(DFkfold)/3):(nrow(DFkfold)*2/3),]
DFkfold3 = DFkfold[(nrow(DFkfold)*2/3):nrow(DFkfold),]

# Create kfold matrix
kfold = matrix(nrow = 3, ncol = 1)

best_comb

# Perform 10 fold cross validation for each DKfold
kfold[1,1] = boosted_kfold(DFkfold1, c(1,2,3,4,5,6,7,9,10))
kfold[2,1] = boosted_kfold(DFkfold2, c(1,2,3,4,5,6,7,9,10))
kfold[3,1] = boosted_kfold(DFkfold3, c(1,2,3,4,5,6,7,9,10))

# Saving model accuracy as "model_accuracy_NN.RData"
model_accuracy_bRF = colMeans(kfold)
# save(model_accuracy_bRF, file = "../Roeser, Jonas - 2_Data/model_accuracy_bRF.RData")


# FINAL MODEL -------------------------------------------------------------

# Since the boosted Random Forest yields the highet 10fold accuracy, we now use it for our final model,
# training it with all of our data!

DFmodel = DF[,c(1,2,3,4,5,6,7,9,10)]
DFmodel[,ncol(DFmodel)] = as.factor(DFmodel[,ncol(DFmodel)])

grid = expand.grid(.n.trees = 400,
                   .interaction.depth = 5,
                   .shrinkage = 0.03,
                   .n.minobsinnode = 2.25
)

control = trainControl(method = "cv",
                       number = 5)

FINALmodel = train(Y ~ .,
              data = DFmodel,
              method = "gbm",
              tuneGrid = grid,
              trControl = control
              )


# Saving FINALmodel accuracy as "FINALmodel.RData"
# save(FINALmodel, file = "../Roeser, Jonas - 2_Data/FINALmodel.RData")

# Other stuff ---------------------
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
best.iter = gbm.perf(temp_model, method="cv")
best.iter
summary(temp_model)


# Need to backscale to original df for visually understandable results
# Plotting the marginal effect of selected variables by "integrating"
# out the other variables
plot.gbm(gbm.model, 1, best.iter)
plot.gbm(gbm.model, 2, best.iter)
plot.gbm(gbm.model, 3, best.iter)
plot.gbm(gbm.model, 4, best.iter)
plot.gbm(gbm.model, 5, best.iter)
plot.gbm(gbm.model, 6, best.iter)
plot.gbm(gbm.model, 7, best.iter)
plot.gbm(gbm.model, 8, best.iter)
plot.gbm(gbm.model, 9, best.iter)


fitControl = trainControl(method="cv", number=5, returnResamp = "all")
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


mResults = predict(model2, test, na.action = na.pass, type = "prob")
mResults$obs = test$Y
head(mResults)

mnLogLoss(mResults, lev = levels(mResults$obs))

mResults$pred = predict(model2, test, na.action = na.pass)
multiClassSummary(mResults, lev = levels(mResults$obs))


evalResults <- data.frame(Class = test$Y)
evalResults$GBM <- predict(model2, test, na.action = na.pass, type = "prob")[,1]
head(evalResults)

trellis.par.set(caretTheme())
liftData <- lift(Class ~ GBM, data = evalResults)
plot(liftData, values = 60, auto.key = list(columns = 1,
                                            lines = TRUE,
                                            points = FALSE))