

require(gbm)
require(MASS) #package with the boston housing dataset

#separating training and test data
train=sample(1:506,size=374)

Boston.boost=gbm(medv ~ . ,data = Boston[train,],distribution = "gaussian",n.trees = 10000,
                 shrinkage = 0.01, interaction.depth = 4)
Boston.boost

summary(Boston.boost) #Summary gives a table of Variable Importance and a plot of Variable Importance

gbm(formula = medv ~ ., distribution = "gaussian", data = Boston[-train, 
                                                                 ], n.trees = 10000, interaction.depth = 4, shrinkage = 0.01)
A gradient boosted model with gaussian loss function.
10000 iterations were performed.
There were 13 predictors of which 13 had non-zero influence.

>summary(Boston.boost)
var     rel.inf
rm           rm 36.96963915
lstat     lstat 24.40113288
dis         dis 10.67520770
crim       crim  8.61298346
age         age  4.86776735
black     black  4.23048222
nox         nox  4.06930868
ptratio ptratio  2.21423811
tax         tax  1.73154882
rad         rad  1.04400159
indus     indus  0.80564216
chas       chas  0.28507720
zn           zn  0.09297068