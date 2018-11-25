
# MERGING EVRYTHING INTO ONE BIG DATAFRAME D

library(tidyverse)

rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/data1991_2017.RData")
load("../Roeser, Jonas - 2_Data/matches.RData")
load("../Roeser, Jonas - 2_Data/rankings.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/data1991_2017.RData")
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
# for (i in 1:nrow(matches)) {
#   if (matches_shuffled$randomisedY[i] == 1) {
#     matches_shuffled$winner_player_id[i] = matches$loser_player_id[i]
#     matches_shuffled$loser_player_id[i] = matches$winner_player_id[i]
#     
#     matches_shuffled$winner_column[i] = matches$loser_column[i]
#     matches_shuffled$loser_column[i] = matches$winner_column[i]
#   }
# }

# --> now we don't need the original (sorted) matches dataframe anymore, so we overwrite it
matches = matches_shuffled
rm(matches_shuffled)

# --> we rename to player0 and player1
colnames(matches) = c("tourney_dates",
                      "match_id",
                      "player0",
                      "player1",
                      "player0_rank_id",
                      "player1_rank_id",
                      "Y")


# Merging matches and rankings --------------------------------------------

matches = merge(matches, rankings, by.x = "player0_rank_id", by.y = "identifier")
matches = merge(matches, rankings, by.x = "player1_rank_id", by.y = "identifier")

# --> reording columns, omitting player_rank_id and week_title
matches = matches[c(3:5,19:15,6,18:23,7)]


# Merging matches & playerOverviews ----------------------------------------

D = merge(matches, playerOverviews, by.x = "player0", by.y = "player_id")
D = merge(D, playerOverviews, by.x = "player1", by.y = "player_id")

# --> reording columns, omitting redundant data (e.g. weight in lbs)
D = D[c(3:4,2,5:10,18,22:25,29,31,34:36,1,11:16,37,41:44,48,50,53:55,17)]


# Merging D & matchScores ----------------------------------------

D = merge(D, matchScores[c(1,5:7,14:23)], by = "match_id")


# Merging D & tournaments ----------------------------------------

D = merge(D, tournaments, by = "tourney_year_id")


# Merging D & matchStats ----------------------------------------

D = merge(D, matchStats, by = "match_id")


# --> tourney order does not match!?

# save(D, file = "../2_Data/D.RData")


# Feature Ideas -----------------------------------------------------------


# Was für features wollen wir?
#   
# Vorhanden
# Weltranglistenrang zum Zeitpunkt des Spiels (W/L)
# Weltranglistenpoints (W/L)
# Untergrund
# Outdoor/Indoor
# Location (Stadt)
# Tournament
# Date
# Series
# Round
# Best of Three/Five
# Name (W/L)
# Spiele in jedem Satz (W/L)
# Losersets
# Quoten
# Age

# 1. Wie brauchen ein Dataset mit allen Spielern und Ihren Geburtsdaten
# 2. Zwei Spalten erstellen, die wir mit ALter füllen wollen (W/L)
# 3. Aus dem Dataset mit Playerdata Geburstdatum nehmen und mit Spieldatum zu einem Alter
#    errechnen (Rechnen mit Dates siehe Script aus Workshop)
# 
# 
# Titel gewonnen:
# Matche gewonnnen total
# Matche verloren total
# Price Money gewonnen
# Titel auf Untergrund (Hardcourt, Sand, Rasen)
# Matchsiege auf Untergrund (Hardcourt, Sand, Rasen)
# Spiel findet in Heimatland statt? -> Binary 
# When turned pro?
# Right hand/left hand
# Single/ double handed backhand
# Percentages of 1. Serve won, 2. Serve won, Break points won etc...
