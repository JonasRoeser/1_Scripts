# QUALIFYING CONDITIONS

# Setup --------------------------------------------------------------

library(tidyverse)

rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/D.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/D.RData")

# First Section yet to be named --------------------------------------

# Forcing these columns to be characters so (sapply and) switch work (otherwise factor problems)
D$tourney_conditions = as.character(D$tourney_conditions)
D$tourney_surface = as.character(D$tourney_surface)

D$move_direction.x = as.character(D$move_direction.x)
D$move_direction.y = as.character(D$move_direction.y)

# making Na to 0 in move directions
D[["move_positions.x"]][is.na(D[["move_positions.x"]])] <- 0
D[["move_positions.y"]][is.na(D[["move_positions.y"]])] <- 0



D <- D %>%
  mutate(tourney_conditions = sapply(D$tourney_conditions, switch, 
                                     Outdoor = 1, 
                                     Indoor = 2) ) %>%           
  mutate(tourney_surface = sapply(D$tourney_surface, switch, 
                                  Clay = 1,
                                  Grass = 2,
                                  Hard = 3,
                                  Carpet = 4)) %>%
  mutate(move_direction.x = sapply(D$move_direction.x, switch, #Still NULLS
                                   up = 1, 
                                   down = (-1) )) %>%
  mutate(move_direction.y = sapply(D$move_direction.y, switch, #Still NULLS
                                   up = 1, 
                                   down = (-1) ))

#  mutate(ranking_move_p0 = move_positions.x * move_direction.x ) #still problem wit NULL



#    mutate(tourney_slug = sapply(D$tourney_slug, switch, 
#                                             "australian-open" = 2000, 
#                                             "french-open" = 2000,
#                                             "us-open" = 2000,
#                                             "wimbledon" = 2000,
#                                                                ) )
# 

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