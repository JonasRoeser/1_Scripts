# PLAYER PROPERTIES

# SETUP -----

library(tidyverse)

rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/D1.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/D1.RData")


# Ceating Feature: Age Difference -----

D1.prop = D1[c(2,29,47,35,53)] %>%
  mutate(age_diff = D1$player_age.x - D1$player_age.y) %>%


# Ceating Feature: Height Difference -----

  mutate(height_diff = D1$height_cm.x - D1$height_cm.y)

D1.prop = D1.prop[c(1,6,7)]


# Ceating Feature: Titel -----

# Gewonnene Titel bis zum Zeitpunkt des Spiels
# 