# ADDING FORM, H2H & CONDITION_WINS TO D; FINALIZING D

# Setup --------------------------------------------------------------

library(tidyverse)
library(dplyr)
rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/D1.RData")
load("../Roeser, Jonas - 2_Data/form.RData")
load("../Roeser, Jonas - 2_Data/condition_wins.RData")
load("../Roeser, Jonas - 2_Data/h2h.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/D1.RData")
load("../2_Data/form.RData")
load("../2_Data/condition_wins.RData")
load("../2_Data/h2h.RData")

set.seed(1)
# Selecting & renaming columns of D1 --------------------------------------------------------------

D1 = D1[,c(27,59,45,60,56)]

colnames(D1) = c("rank_number_p0",
                 "ranking_move_p0",
                 "rank_number_p1",
                 "ranking_move_p1",
                 "Y")


# Adding form, h2h & condition_wins ---------------------------------------

D1 = cbind(D1, form[,c(6,7)], h2h[,c(11,12)], condition_wins[,c(8,9)])


# Ordering D1 ---------------------------------------

# Ordering and selecting relevant features
D1 = D1[,c(1,2,6,8,10,3,4,7,9,11,5)]

# separate Y from df
Y = D1$Y


# Standardizing D1 ---------------------------------------

# We dont want to standardize Y
D1 = scale(D1[1:10])
# --> maybe this needs to be done manually in order to be able to standardise the fornt-end input!

# Binding D1 to Y
D1 = cbind(D1,Y)

# Removing NAs ---------------------------------------

D1 = D1[which(!is.na(D1[,3]) &
                !is.na(D1[,5]) &
                !is.na(D1[,8]) &
                !is.na(D1[,10])),]

# Creating a DF and a shuffled version of DF --------------------
DF = D1
DFs = sample_n(as.data.frame(D1),nrow(D1))

# Saving -------------------------------------------------------------


# save(DF, file = "../Roeser, Jonas - 2_Data/DF.RData")
# save(DFs, file = "../Roeser, Jonas - 2_Data/DFs.RData")