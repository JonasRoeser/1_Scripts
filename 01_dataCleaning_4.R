#MERGE MERGE MERGE

library(tidyverse)

rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/data1991_2017.RData")
load("../Roeser, Jonas - 2_Data/matches_W_rkns.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/data1991_2017.RData")
load("../2_Data/matches_W_rkns.RData")


# --- reording columns, omitting player_rank_id and week_title
matches_w_rkns = matches_w_rkns[c(3:5,10:15,6,18:23,7)]

# adding a match sequence id, so we can resort our data when the merging makes everything messy
matches_w_rkns$match_sq = 1:nrow(matches_w_rkns)


# Merging matches & playerOverviews ----------------------------------------

D = merge(matches_w_rkns, playerOverviews, by.x = "player0", by.y = "player_id")
D = merge(D, playerOverviews, by.x = "player1", by.y = "player_id")

# --> reording columns, omitting redundant data (e.g. weight in lbs)
#D = D[c(3:4,2,5:10,18,22:25,29,31,34:36,1,11:16,37,41:44,48,50,53:55,17)]


# Merging D & matchScores ----------------------------------------

D = merge(D, matchScores[c(1,5:7,14:23)], by = "match_id")


# Merging D & tournaments ----------------------------------------

D = merge(D, tournaments, by = "tourney_year_id")


# Merging D & matchStats ----------------------------------------

D = merge(D, matchStats, by = "match_id")

# Omitting all redundant or utterly useless colums (e.g website links) and resorting
#all match stats dropped except match duration ( match stats would be colnr(61,62,64:73, 102:148))
D = D[c(19,5,1,2,74,75,79,81:83,85,87,88,58:60, 63,100,4,20:22,24:26,6:11,31,33,36:38,3,39:41,43:45,12:17,50, 52, 55:57,18)]



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