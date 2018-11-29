# CREATING A DF VERSION WITH ONLY THE DIFFERENCE IN THE FEATURES

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



# Removing NAs ---------------------------------------

D1 = D1[which(!is.na(D1[,3]) &
                !is.na(D1[,5]) &
                !is.na(D1[,8]) &
                !is.na(D1[,10])),]

# separate Y from df
Y = D1$Y

# Creating DF with difference features(totally original idea!!!)
D1_test = D1 %>%
  mutate(diff_rk = D1$rank_number_p0 - D1$rank_number_p1) %>%
  mutate(diff_form = D1$form_player0 - D1$form_player1) %>%
  mutate(diff_h2h = D1$h2h_v3_player0) %>%
  mutate(diff_c_wins = D1$condition_wins_player0 - D1$condition_wins_player1)

D1 = D1_test[,c(12:15)]

# Standardizing
D1 = scale(D1[1:4])

# Binding D1 to Y
D1 = cbind(D1,Y)


# Creating a DF_test and a shuffled version of DF_test --------------------
DF_test = D1
DFs_test = sample_n(as.data.frame(D1),nrow(D1))

# Saving -------------------------------------------------------------


# save(DF_test, file = "../Roeser, Jonas - 2_Data/DF_test.RData")
# save(DFs_test, file = "../Roeser, Jonas - 2_Data/DFs_test.RData")