
# CREATING FORM FEATURE FOR D

library(tidyverse)

rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/D1.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/D1.RData")


# Creating form feature ---------------------------------------------------

D1 = D1[c(58,2,20,38,56)] %>%
  mutate(form_player0 = NA) %>%
  mutate(form_player1 = NA)

for (i in 1:nrow(D1)) {
  # Calculating Form for player0
  matches0 = which(as.character(D1$player0) == as.character(D1$player0[i]))
  matches1 = which(as.character(D1$player1) == as.character(D1$player0[i]))
  matches_smaller_i_indeces0 = which(matches0<i)
  matches_smaller_i_indeces1 = which(matches1<i)
  matches_smaller_i0 = matches0[matches_smaller_i_indeces0]
  matches_smaller_i1 = matches1[matches_smaller_i_indeces1]
  matches_smaller_iboth = sort(append(matches_smaller_i1, matches_smaller_i0))
  
  n_matches0 = length(matches_smaller_i0)
  n_matches1 = length(matches_smaller_i1)
  n_matches = length(matches_smaller_iboth)
  if (n_matches >= 10) {
    matchResults = D1$Y[matches_smaller_iboth[c((n_matches)-9):n_matches]]
    
    matchesInPlayer0Column = match(matches_smaller_i0[c((n_matches0-9):n_matches0)[which(c((n_matches0-9):n_matches0)>0)]],
                                   matches_smaller_iboth[c((n_matches)-9):n_matches])
    matchesInPlayer0ColumnNoNA = matchesInPlayer0Column[which(!is.na(matchesInPlayer0Column))]
    
    matchesInPlayer1Column = match(matches_smaller_i1[c((n_matches1-9):n_matches1)[which(c((n_matches1-9):n_matches1)>0)]],
                                   matches_smaller_iboth[c((n_matches)-9):n_matches])
    matchesInPlayer1ColumnNoNA = matchesInPlayer1Column[which(!is.na(matchesInPlayer1Column))]
    
    D1$form_player0[i] = length(which(matchResults[matchesInPlayer0ColumnNoNA] == 0)) + length(which(matchResults[matchesInPlayer1ColumnNoNA] == 1))
  } else {
    D1$form_player0[i] = NA
  }
  
  # Calculating form for player1
  matches0 = which(as.character(D1$player0) == as.character(D1$player1[i]))
  matches1 = which(as.character(D1$player1) == as.character(D1$player1[i]))
  matches_smaller_i_indeces0 = which(matches0<i)
  matches_smaller_i_indeces1 = which(matches1<i)
  matches_smaller_i0 = matches0[matches_smaller_i_indeces0]
  matches_smaller_i1 = matches1[matches_smaller_i_indeces1]
  matches_smaller_iboth = sort(append(matches_smaller_i1, matches_smaller_i0))
  
  n_matches0 = length(matches_smaller_i0)
  n_matches1 = length(matches_smaller_i1)
  n_matches = length(matches_smaller_iboth)
  if (n_matches >= 10) {
    matchResults = D1$Y[matches_smaller_iboth[c((n_matches)-9):n_matches]]
    
    matchesInPlayer0Column = match(matches_smaller_i0[c((n_matches0-9):n_matches0)[which(c((n_matches0-9):n_matches0)>0)]],
                                   matches_smaller_iboth[c((n_matches)-9):n_matches])
    matchesInPlayer0ColumnNoNA = matchesInPlayer0Column[which(!is.na(matchesInPlayer0Column))]
    
    matchesInPlayer1Column = match(matches_smaller_i1[c((n_matches1-9):n_matches1)[which(c((n_matches1-9):n_matches1)>0)]],
                                   matches_smaller_iboth[c((n_matches)-9):n_matches])
    matchesInPlayer1ColumnNoNA = matchesInPlayer1Column[which(!is.na(matchesInPlayer1Column))]
    
    D1$form_player1[i] = length(which(matchResults[matchesInPlayer0ColumnNoNA] == 0)) + length(which(matchResults[matchesInPlayer1ColumnNoNA] == 1))
  } else {
    D1$form_player1[i] = NA
  }
}


# Saving -------------------------------------------------------------

D2 = D1
# save(D2, file = "../Roeser, Jonas - 2_Data/D2.RData")
D2noNA = D1[which(!is.na(D1$form_player1) & !is.na(D1$form_player0)),]
# save(D2noNA, file = "../Roeser, Jonas - 2_Data/D2noNA.RData")