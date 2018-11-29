# QUANTIFYING SELECTED COLUMNS OF D

# Setup --------------------------------------------------------------

library(tidyverse)

rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/data1991_2017.RData")
load("../Roeser, Jonas - 2_Data/D0.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/data1991_2017.RData")
load("../2_Data/D0.RData")


# Quantifying playing conditions --------------------------------------------------

# Forcing these colums to be characters so (sapply and) switch work (otherwise factor problems)

D0 <- D0 %>%
  mutate(tourney_conditions = sapply(as.character(D0$tourney_conditions), switch, 
                                     Outdoor = 1, 
                                     Indoor = 0) ) %>%           
  mutate(tourney_surface = sapply(as.character(D0$tourney_surface), switch, 
                                  Clay = 1,
                                  Grass = 2,
                                  Hard = 3,
                                  Carpet = 4))


# Quantifying move directions --------------------------------------------------

# To avoid getting NULLS later
D0$move_direction.x = as.double(D0$move_direction.x)
D0$move_direction.y = as.double(D0$move_direction.y)

# Forcing these colums to be characters so (sapply and) switch work (otherwise factor problems)
D0$move_direction.x = as.character(D0$move_direction.x)
D0$move_direction.y = as.character(D0$move_direction.y)

# Making NA to 0 in move directions
D0[["move_positions.x"]][is.na(D0[["move_positions.x"]])] <- 0
D0[["move_positions.y"]][is.na(D0[["move_positions.y"]])] <- 0

D0 <- D0 %>%
  mutate(move_direction.x = sapply(D0$move_direction.x, switch, 
                                   "3" = 1,
                                   "2" = (-1),
                                   "1" = 0)) %>%
  mutate(move_direction.y = sapply(D0$move_direction.y, switch, 
                                   "3" = 1, 
                                   "2" = (-1),
                                   "1" = 0)) %>%
  mutate(ranking_move_p0 = move_positions.x * move_direction.x ) %>%
  mutate(ranking_move_p1 = move_positions.y * move_direction.y )



# Quantifying round name --------------------------------------------------

D0$tourney_round_name = as.double(D0$tourney_round_name)


# Saving -------------------------------------------------------------

# Saving "D1" as D1.RData
D1 = D0
# save(D, file = "../Roeser, Jonas - 2_Data/D1.RData")


# Feature Ideas -----------------------------------------------------------

# Was f?r features wollen wir?
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
# 2. Zwei Spalten erstellen, die wir mit ALter f?llen wollen (W/L)
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