# MERGE MERGE MERGE

# Setup --------------------------------------------------------------

library(tidyverse)

rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/data1991_2017.RData")
load("../Roeser, Jonas - 2_Data/matches_W_rkns.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/data1991_2017.RData")
load("../2_Data/matches_W_rkns.RData")

# Data preparation ---------------------------------------------------

# Reording columns, omitting "player_rank_id" and "week_title"
matches_w_rkns = matches_w_rkns[c(3:5,10:15,6,18:23,7)]

# Adding a match sequence id, so we can resort our data when the merging messes up the order
matches_w_rkns$match_sq = 1:nrow(matches_w_rkns)

# Merging dataframes -------------------------------------------------

# Merging "matches" & "playerOverviews"
D = merge(matches_w_rkns, playerOverviews, by.x = "player0", by.y = "player_id")
D = merge(D, playerOverviews, by.x = "player1", by.y = "player_id")

# Merging "D" & "matchScores"
D = merge(D, matchScores[c(1,5:7,14:23)], by = "match_id")

# Merging "D" & "tournaments"
D = merge(D, tournaments, by = "tourney_year_id")

# Merging "D" & "matchStats" 
D = merge(D, matchStats, by = "match_id")

# Omitting all redundant or utterly useless colums (e.g website links) and resorting
# all match stats dropped except match duration ( match stats would be colnr(61,62,64:73, 102:148))
D = D[c(19,5,1,2,74,75,79,81:83,85,87,88,58:60,63,100,4,20:22,24:26,6:11,31,33,36:38,3,39:41,43:45,12:17,50,52,55:57,18)]


# Saving -------------------------------------------------------------

# Saving "D" as D.RData
save(D, file = "../Roeser, Jonas - 2_Data/D.RData")