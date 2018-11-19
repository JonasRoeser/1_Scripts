library(tidyverse)
library(dplyr)

rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/data1991_2017.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/data1991_2017.RData")


# Checking for columns that allow us to merge the dataframes --------------
sapply(tournaments, class) # tourney_year_id "factor", tourney_dates "factor"
sapply(matchScores, class) # match_id "factor, tourney_year_id "factor"
sapply(matchStats, class) # match_id "factor"
sapply(rankings, class) # week_title "factor", player_id "factor"
sapply(playerOverviews, class) # player_id "factor", 

# Furthermore: in our data the winner is always the first player of the two!


# Selecting relevant columns from matchScores -----------------------------

D = subset(matchScores, select = c(tourney_year_id,
                                    match_id,
                                    winner_player_id,
                                    loser_player_id))

# Merging matchScores & rankings ------------------------------------------

# In order to match matchScores & rankings we need to match matchScores & torunaments.
# This will give us a tourney_dates column, that can then be used to match matchScores
# rankings.
D = subset(merge(D, tournaments, by = "tourney_year_id"), select = c(tourney_dates,
                                                                     match_id,
                                                                     winner_player_id,
                                                                     loser_player_id))

# In order to be able to match matchScores & rankings we match the new column in D
# tourney_year_id, with the column week_title in rankings.
# However, we have to convert the dates into numeric format first. We do that by substituting
# "." for "-" so that we have them in format "xxxx-xx-xx". Then we convert them into dates,
# before finally converting them into numericals.
D$tourney_dates = as.numeric(as.Date(chartr(".", "-", D$tourney_dates)))
D = D[order(D$tourney_dates),] # Ordering data according to date
rownames(D) = 1:nrow(D)

# We do the same thing for rankings. But we select the relevant columns week_title, player_id
# rank_number & ranking_points first.
R = subset(rankings, select = c(week_title,
                                player_id,
                                rank_number,
                                ranking_points,
                                player_age,
                                tourneys_played,
                                move_positions,
                                move_direction))

R$week_title = as.numeric(as.Date(chartr(".", "-", R$week_title)))
R = R[order(R$week_title),] # Ordering data according to date
rownames(R) = 1:nrow(R)

# We create new columns of NAs that we can later fill up in the for loop
D$winner_player_rank = NA
D$winner_ranking_points = NA
D$winner_player_age = NA
D$winner_tourneys_played = NA
D$winner_move_positions = NA
D$winner_move_direction = NA

D$loser_player_rank = NA
D$loser_ranking_points = NA
D$loser_player_age = NA
D$loser_tourneys_played = NA
D$loser_move_positions = NA
D$loser_move_direction = NA

for(i in 1:nrow(D)) {
  i=500
  sth = R %>%
    filter(week_title < D[i,1])
  
  sth = sth %>%
    filter(sth[,1] == max(sth[,1]))
  
  sth1 = sth %>%
    filter(as.character(sth[,2]) == as.character(D[i,3]))
  
  D$winner_player_rank[i] = sth1[1,3]
  D$winner_ranking_points[i] = sth1[1,4]
  D$winner_player_age[i] = sth1[1,5]
  D$winner_tourneys_played[i] = sth1[1,6]
  D$winner_move_positions[i] = sth1[1,7]
  D$winner_move_direction[i] = sth1[1,8]
  
  sth2 = sth %>%
    filter(as.character(sth[,2]) == as.character(D[i,4]))
  
  D$loser_player_rank[i] = sth2[1,3]
  D$loser_ranking_points[i] = sth2[1,4]
  D$loser_player_age[i] = sth2[1,5]
  D$loser_tourneys_played[i] = sth2[1,6]
  D$loser_move_positions[i] = sth2[1,7]
  D$loser_move_direction[i] = sth2[1,8]
}

save(D, file = "../2_Data/D.RData")

# Merging matchScores & matchStats ----------------------------------------
merged = merge(matchScores, matchStats, by = "match_id") # --> tourney order does not match!?
merged = subset(merged, select = -c(5,24,26))

# Selecting the appropriate features --------------------------------------
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


