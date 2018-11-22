#MERGE MERGE MERGE

library(tidyverse)

rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/data1991_2017.RData")
load("../Roeser, Jonas - 2_Data/matches_shuffled.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/data1991_2017.RData")
load("../2_Data/matches_shuffled.RData")




# Merging matches_shuffled & playerOverviews ----------------------------------------

D = merge(matches_shuffled, playerOverviews, by.x = "player0", by.y = "player_id")
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
