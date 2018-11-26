library(ggplot2)
library(cowplot)
library(randomForest)

rm(list = ls())
load("../Roeser, Jonas - 2_Data/matches_w_rkns.RData")


test = matches_w_rkns[c(10,18,7)]


# build a random forest--------

set.seed(1)


#so we dont get fuggin 70 gb of randome forests
test = test[1:15000,] #we cannot take the whole dataset because R will not compute files that big


test$Y = as.factor(test$Y)
class(test$Y)
model <- randomForest(Y ~ ., data=test,ntree=500, proximity=TRUE) #starting point was 500 trees and the OOB error was also 39 %
# -> not real improvements
## now see what the OOB error rate is
model 

#number of trees is 500 -> will there be any improvement if we make more trees?

#plotting the error rates
oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "1", "0"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"],
          model$err.rate[,"1"],
          model$err.rate[,"0"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))


#trying it with 1000 trees
model <- randomForest(Y ~ ., data=test, ntree=1000, proximity=TRUE)
model
#OOB error rate has not improved (still around 39,11 %)

#finding the optimal number of variable at each internal nodes

oob.values <- vector(length=10)

for(i in 1:10) {
  temp.model <- randomForest(Y ~ ., data=test, mtry=i, ntree=1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
oob.values

# this is 1 (obviously bcause we only had 2 features)


## NOW: an MDS-plot to show how the samples are related to each
## other.
##
## Start by converting the proximity matrix into a distance matrix.
distance.matrix <- dist(1-model$proximity)

mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)

## calculate the percentage of variation that each MDS axis accounts for...
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

## now make a fancy looking plot that shows the MDS axes and the variation:
mds.values <- mds.stuff$points
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