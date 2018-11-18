rm(list = ls())
load("../Roeser, Jonas - 2_Data/data1991_2017.RData")
load("../2_Data/data1991_2017.RData")
DF_test = subset(matchScores, select = -c(tourney_order,tourney_url_suffix,
                                          tourney_year_id, tourney_slug,
                                          tourney_round_name, 
                                          round_order, 
                                          match_order, 
                                          winner_name, 
                                          loser_name, 
                                          winner_sets_won,
                                          loser_sets_won,
                                          winner_games_won,
                                          loser_games_won,
                                          winner_tiebreaks_won,
                                          loser_tiebreaks_won
                                          ) )
