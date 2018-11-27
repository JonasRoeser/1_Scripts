# RANDOM FORESTS WITH BAGGING


# Setup-------------------------

library(ggplot2)
library(cowplot)
library(randomForest)

rm(list = ls())

load("../Roeser, Jonas - 2_Data/U.RData")
load("../2_Data/U.RData")


# Preperation ------------

set.seed(1)


# Data Formatting
U1 = U[c(5,7,9,11,12)]

U1 = na.omit(U1)


# We cannot take the whole dataset because of computational restrictions (so we dont get 70 gb)
train = U1[1:5000,] 
test = U1[5001:10000,]

train$Y.x = as.factor(train$Y.x)
test$Y.x = as.factor(test$Y.x)

# Building and Training Model ----------------------------------------

# Finding the optimal number of variable at each internal nodes
oob.values = vector(length=4)

for(i in 1:4) {
  temp.model = randomForest(Y.x ~ ., data=train, mtry=i, ntree=1000)
  oob.values[i] = temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values
# --> We can see that mtry=1 yields the lowest OOB error rate

# Running the random forest with the built in function of the 
model = randomForest(Y.x ~ ., data=train, mtry =1, ntree=1000, proximity=TRUE) 
# --> Looking at "model" we can see what the OOB error rate is .......

# Plotting the error rate to see how many trees are necessary
oob.error.data = data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "1", "0"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"],
          model$err.rate[,"1"],
          model$err.rate[,"0"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))
# --> We can see that in this particular case the Error rate is ......

# Getting probabilities predictions ----------------------------------
predictions = as.data.frame(predict(model, train, type = "prob"))


# More plotting shizzel that I do not understand -------------
# converting the proximity matrix into a distance matrix.
distance.matrix <- dist(1-model$proximity)

mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)

#the percentage of variation that each MDS axis accounts for...
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

## trying to make fancy looking plot that shows the MDS axes and the variation:
mds.values <- mds.stuff$points #spoiler: takes forever to load
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Status=test$hd)

ggplot(data=mds.data, aes(x=X, y=Y, label=Sample)) +
  geom_text(aes(color=Status)) +
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using (1 - Random Forest Proximities)")