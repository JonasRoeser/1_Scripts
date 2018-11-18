load("D.RData")
D = D %>%
  mutate(Y = 1)
Xtrain = as.matrix(D[1:15000, 5:6])
Ytrain = as.matrix(D[1:15000, 7])
Xtest= as.matrix(D[15001:22000, 5:6])
Ytest = as.matrix(D[15001:22000, 7])

data <- data.frame(Ytrain, Xtrain)
model <- glm(Ytrain ~ ., data, family=binomial(link='logit'))

beta_logistic  = model$coefficients

cbind(1,Xtest) %*% beta_logistic 
