# ADDING FORM, H2H & CONDITION_WINS TO D; FINALIZING D

# Setup --------------------------------------------------------------

library(tidyverse)

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


# Selecting & renaming columns of D1 --------------------------------------------------------------

D1 = D1[,c(27,59,45,60,56)]

colnames(D1) = c("rank_number_p0",
                 "ranking_move_p0",
                 "rank_number_p1",
                 "ranking_move_p1",
                 "Y")


# Adding form, h2h & condition_wins ---------------------------------------

D1 = cbind(D1, form[,c(6,7)], h2h[,c(5:12)], condition_wins[,c(8,9)])


# Ordering D1 ---------------------------------------

D1 = D1[,c(1,2,6,8,11,14,16,3,4,7,9,12,15,17,10,13,5)]


# Standardising D1 ---------------------------------------

D1 = scale(D1)

# --> maybe this needs to be done manually in order to be able to standardise the fornt-end input!


# Removing NAs ---------------------------------------

D1 = D1[which(!is.na(D1[,3]) &
                !is.na(D1[,7]) &
                !is.na(D1[,10]) &
                !is.na(D1[,14])),]


# Saving -------------------------------------------------------------

DF = D1
# save(DF, file = "../Roeser, Jonas - 2_Data/DF.RData")