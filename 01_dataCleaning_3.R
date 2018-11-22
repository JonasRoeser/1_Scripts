# MERGING EVRYTHING INTO ONE BIG DATAFRAME D

library(tidyverse)

rm(list = ls())

# Reading the previously saved version of our data
#load("../Roeser, Jonas - 2_Data/data1991_2017.RData")
load("../Roeser, Jonas - 2_Data/matches.RData")
load("../Roeser, Jonas - 2_Data/rankings.RData")

# Because of OneDrive we need to load from two different paths
#load("../2_Data/data1991_2017.RData")
load("../2_Data/matches.RData")
load("../2_Data/rankings.RData")


# Reordering winners & losers to randomised Y -----------------------------

set.seed(1)
matches = matches %>%
  mutate(randomisedY = sample(c(0,1), replace=TRUE, size=nrow(matches)))

# --> we need the original matches dataframe for switching the columns in the for-loop
matches_shuffled = matches

# we now randomise our results, so that a 0 indicates the player0 winning whilst a 1
# indicates the first player1 winning!
for (i in 1:nrow(matches)) {
  if (matches_shuffled$randomisedY[i] == 1) {
    matches_shuffled$winner_player_id[i] = matches$loser_player_id[i]
    matches_shuffled$loser_player_id[i] = matches$winner_player_id[i]
    
    matches_shuffled$winner_column[i] = matches$loser_column[i]
    matches_shuffled$loser_column[i] = matches$winner_column[i]
  }
}


# --> we rename to player0 and player1
colnames(matches_shuffled) = c("tourney_dates",
                               "match_id",
                               "player0",
                               "player1",
                               "player0_rank_id",
                               "player1_rank_id",
                               "Y")


# Merging matches and rankings --------------------------------------------

matches_shuffled = merge(matches_shuffled, rankings, by.x = "player0_rank_id", by.y = "identifier")
matches_shuffled = merge(matches_shuffled, rankings, by.x = "player1_rank_id", by.y = "identifier")

# --> reording columns, omitting player_rank_id and week_title
matches_shuffled = matches_shuffled[c(3:5,19:15,6,18:23,7)]

#save

save(matches_shuffled, file = "../Roeser, Jonas - 2_Data/matches_shuffled.RData")
