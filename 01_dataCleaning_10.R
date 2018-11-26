
# CREATING NEW FEATURES FOR D

library(tidyverse)

rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/D.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/D.RData")

D = D[c(1:4,19,37,55)] %>%
  mutate(form_player0 = NA) %>%
  mutate(form_player1 = NA)

D = D%>%
  mutate(mcFreezy = paste(D$player0, D$player1, D$player0, sep = "-"))

for (i in 87000:nrow(D)) {
  matches1 = which(as.character(D$player1) == as.character(D$player1[i]))
  matches0 = which(as.character(D$player0) == as.character(D$player1[i]))
  matches_smaller_i_indeces1 = which(matches0<i)
  matches_smaller_i_indeces0 = which(matches0<i)
  matches_smaller_i1 = matches1[matches_smaller_i_indeces1]
  matches_smaller_i0 = matches0[matches_smaller_i_indeces0]
  matches_smaller_iboth = sort(append(matches_smaller_i1, matches_smaller_i0))
  
  n_matches = length(matches_smaller_iboth)
  if (n_matches >= 10) {
    sth = D$Y[matches_smaller_iboth[c((n_matches)-9):n_matches]]
    bruv0 = match(matches_smaller_i1[c((n_matches)-9):n_matches], matches_smaller_iboth[c((n_matches)-9):n_matches])
    sth0 = bruv0[which(!is.na(bruv0))]
    bruv1 = match(matches_smaller_i1[c((n_matches)-9):n_matches], matches_smaller_iboth[c((n_matches)-9):n_matches])
    sth1 = bruv1[which(!is.na(bruv1))]
    D$form_player1[i] = length(which(sth[sth0] == 0)) + length(which(sth[sth1] == 1))
  } else {
    D$form_player1[i] = NA
  }
  
  matches1 = which(as.character(D$player1) == as.character(D$player0[i]))
  matches0 = which(as.character(D$player0) == as.character(D$player0[i]))
  matches_smaller_i_indeces1 = which(matches0<i)
  matches_smaller_i_indeces0 = which(matches0<i)
  matches_smaller_i1 = matches1[matches_smaller_i_indeces1]
  matches_smaller_i0 = matches0[matches_smaller_i_indeces0]
  matches_smaller_iboth = sort(append(matches_smaller_i1, matches_smaller_i0))
  
  n_matches = length(matches_smaller_iboth)
  if (n_matches >= 10) {
    sth = D$Y[matches_smaller_iboth[c((n_matches)-9):n_matches]]
    bruv0 = match(matches_smaller_i1[c((n_matches)-9):n_matches], matches_smaller_iboth[c((n_matches)-9):n_matches])
    sth0 = bruv0[which(!is.na(bruv0))]
    bruv1 = match(matches_smaller_i1[c((n_matches)-9):n_matches], matches_smaller_iboth[c((n_matches)-9):n_matches])
    sth1 = bruv1[which(!is.na(bruv1))]
    D$form_player0[i] = length(which(sth[sth0] == 0)) + length(which(sth[sth1] == 1))
  } else {
    D$form_player0[i] = NA
  }
}