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
#9 Percentages of 1. Serve won, 2. Serve won, Break points won etc...

library("tidyverse")
library("dplyr")

rm(list = ls())

# Reading all of our data of the selected time period
matchScores1991_2016 = read.csv("../2_Data/Downloaded Potential Data/match_scores_1991-2016_unindexed_csv.csv")
matchScores2017 = read.csv("../2_Data/Downloaded Potential Data/match_scores_2017_unindexed_csv.csv")
matchStats1991_2016 = read.csv("../2_Data/Downloaded Potential Data/match_stats_1991-2016_unindexed_csv.csv")
matchStats2017 = read.csv("../2_Data/Downloaded Potential Data/match_stats_2017_unindexed_csv.csv")
playerOverviews = read.csv("../2_Data/Downloaded Potential Data/player_overviews_unindexed_csv.csv")
rankings1973_2017 = read.csv("../2_Data/Downloaded Potential Data/rankings_1973-2017_csv.csv")
tournaments1877_2017 = read.csv("../2_Data/Downloaded Potential Data/tournaments_1877-2017_unindexed_csv.csv")

# Because of OneDrive 
matchScores1991_2016 = read.csv("../Roeser, Jonas - 2_Data/Downloaded Potential Data/match_scores_1991-2016_unindexed_csv.csv")
matchScores2017 = read.csv("../Roeser, Jonas - 2_Data/Downloaded Potential Data/match_scores_2017_unindexed_csv.csv")
matchStats1991_2016 = read.csv("../Roeser, Jonas - 2_Data/Downloaded Potential Data/match_stats_1991-2016_unindexed_csv.csv")
matchStats2017 = read.csv("../Roeser, Jonas - 2_Data/Downloaded Potential Data/match_stats_2017_unindexed_csv.csv")
playerOverviews = read.csv("../Roeser, Jonas - 2_Data/Downloaded Potential Data/player_overviews_unindexed_csv.csv")
rankings1973_2017 = read.csv("../Roeser, Jonas - 2_Data/Downloaded Potential Data/rankings_1973-2017_csv.csv")
tournaments1877_2017 = read.csv("../Roeser, Jonas - 2_Data/Downloaded Potential Data/tournaments_1877-2017_unindexed_csv.csv")

# Stats Check --------------------------------------------------------

<<<<<<< HEAD
# Checking if colnames of matchStats match
=======
<<<<<<< HEAD
<<<<<<< HEAD
# Cutting rankings1973_2017 -----------------------------------------------
typeof(rankings1973_2017$week_year)
rankings1991_2017 <- rankings1973_2017 %>%
  filter(week_year >= 1991)
# checking
min(rankings1991_2017$week_year)
# complete?
nrow(is.na(rankings1991_2017$week_year))


# Cutting tournaments1877_2017 --------------------------------------------
typeof(tournaments1877_2017$tourney_year)
tournaments1991_2017 <- tournaments1877_2017 %>%
  filter(tourney_year >= 1991)
# checking
min(tournaments1991_2017$tourney_year)
# complete?
nrow(is.na(tournaments1991_2017$tourney_year))
=======
=======
>>>>>>> 54e30177e8d463d130cd3b64aa0bb009e9f00996
# Stats check ------
>>>>>>> 873f4aec42fdc72d8b763ee1f21e189440b6711a
colnames(matchStats1991_2016) == colnames(matchStats2017)              # All true
# Checking if classes of matchStats columns match
sapply(matchStats1991_2016, class) == sapply(matchStats2017, class)    # All true
# Checking if types of matchStats columns match
sapply(matchStats1991_2016, typeof) == sapply(matchStats2017, typeof)  # All true

# Combining matchStats
matchStats1991_2017 = rbind(matchStats1991_2016, matchStats2017)

<<<<<<< HEAD
# Scores Check -------------------------------------------------------
=======
<<<<<<< HEAD
>>>>>>> 54e30177e8d463d130cd3b64aa0bb009e9f00996
=======
>>>>>>> 54e30177e8d463d130cd3b64aa0bb009e9f00996
>>>>>>> 873f4aec42fdc72d8b763ee1f21e189440b6711a

# Checking if colnames of matchScores match
colnames(matchScores1991_2016) == colnames(matchScores2017)
# Checking if classes of matchScores columns match
sapply(matchScores1991_2016, class) == sapply(matchScores2017, class)
# Checking if types of matchScores columns match
sapply(matchScores1991_2016, typeof) == sapply(matchScores2017, typeof)

# Combining matchScores
matchScores1991_2017 = rbind(matchScores1991_2016, matchScores2017)

# Removing other datasets
rm(matchStats1991_2016, matchStats2017, matchScores1991_2016, matchScores2017)
