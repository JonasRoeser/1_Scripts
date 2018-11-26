# CREATING HEAD TO HEAD COMPARISON

# Preparation

library(tidyverse)

rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/D.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/D.RData")

# Create three columns for h2h ...

D = D[c(59,21,39,57,3)] %>%
  mutate(h2h_player0 = 0) %>%
  mutate(h2h_player1 = 0) %>%
  mutate(h2h = 0)

for (i in 1:nrow(D)) {
  # Gives us all rows of specific p0 vs p1 matches (and the other way around)
  m_p0vp1 = which(as.character(D$player1) == as.character(D$player1[i]) & as.character(D$player0) == as.character(D$player0[i]))
  m_p1vp0 = which(as.character(D$player0) == as.character(D$player1[i]) & as.character(D$player1) == as.character(D$player0[i]))

  smaller_i_i0 = which(m_p0vp1 < i)
  smaller_i_i1 = which(m_p1vp0 < i)
  smaller_i0 = m_p0vp1[smaller_i_i0]
  smaller_i1 = m_p1vp0[smaller_i_i1]
  
  smaller_iboth = sort(append(smaller_i0, smaller_i1))
  D$h2h[i] = length(smaller_iboth)
  
  for (j in 1:length(smaller_i0)) {
    if (length(smaller_i0) == 0) {
      D$h2h_player0[i] = 0
      D$h2h_player1[i] = 0
    } else if (D[smaller_i0[j],4] == 0) {
      D$h2h_player0[i] = D$h2h_player0[i] + 1
    } else {
      D$h2h_player1[i] = D$h2h_player1[i] + 1
    }
  }
  for (j in 1:length(smaller_i1)) {
    if (length(smaller_i1) == 0) {
      
    } else if (D[smaller_i1[j],4] == 1) {
      D$h2h_player0[i] = D$h2h_player0[i] + 1
    } else {
      D$h2h_player1[i] = D$h2h_player1[i] + 1
    }
  }
}
h2h = D
save(h2h, file = "../Roeser, Jonas - 2_Data/h2h.RData")


