library(tidyverse)
library(dplyr)

rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/D.RData")
load("../Roeser, Jonas - 2_Data/R.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/D.RData")
load("../2_Data/R.RData")


merged = merge(D, R, by.x = "winner_column", by.y = "identifier")
merged = merge(merged, R, by.x = "loser_column", by.y = "identifier")

merged = merged[,c("tourney_dates",
                   "match_id",
                   "winner_player_id",
                   "rank_number.x",
                   "loser_player_id",
                   "rank_number.y")]

merged = merged %>%
  mutate(randomisedY = sample(c(0,1), replace=TRUE, size=nrow(merged)))

merged1 = merged

# we now randomise our results, so that a 0 indicates the 2nd player (y) winning whilst a 1
# indicates the first player (x) winning!

# for (i in 1:nrow(merged)) {
#   if (merged1$randomisedY[i] == 0) {
#     merged1$winner_player_id[i] = merged$loser_player_id[i]
#     merged1$loser_player_id[i] = merged$winner_player_id[i]
#     
#     merged1$rank_number.x[i] = merged$rank_number.y[i]
#     merged1$rank_number.y[i] = merged$rank_number.x[i]
#   }
# }

Xtrain = as.matrix(merged1[1:(0.7*nrow(merged1)), c(4,6)])
Ytrain = as.matrix(merged1[1:(0.7*nrow(merged1)), 7])
Xtest= as.matrix(merged1[(0.7*nrow(merged1)+1):nrow(merged1), c(4,6)])
Ytest = as.matrix(merged1[(0.7*nrow(merged1)+1):nrow(merged1), 7])

data <- data.frame(Ytrain, Xtrain)
model <- glm(Ytrain ~ ., data, family=binomial(link='logit'))

beta_logistic  = model$coefficients

model$fitted.values
cbind(1,Xtest) %*% beta_logistic
