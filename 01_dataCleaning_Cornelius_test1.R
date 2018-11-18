rm(list = ls())
load("../Roeser, Jonas - 2_Data/data1991_2017.RData")
load("../2_Data/data1991_2017.RData")
DF_test = subset(matchScores, select = -c(tourney_order,
                                          tourney_url_suffix,
                                          tourney_slug,
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
                                          loser_tiebreaks_won,
                                          winner_slug,
                                          loser_slug,
                                          loser_seed,
                                          winner_seed,
                                          match_score_tiebreaks,
                                          match_stats_url_suffix
                                          ))
DF_test = merge(DF_test, tournaments, by = "tourney_year_id")
DF_test = DF_test[,c(2:4,11)]
DF_test$tourney_dates = chartr(".", "-", DF_test$tourney_dates)
DF_test$tourney_dates = as.numeric(as.Date(DF_test$tourney_dates))

rankings2 = subset(rankings, select = c(ranking_points, player_id, week_title, rank_number))
rankings2$week_title = chartr(".", "-", rankings2$week_title)
rankings2$week_title = as.numeric(as.Date(rankings2$week_title))

library(tidyverse)
DF_test = DF_test %>%
  mutate(player1rank = NA) %>%
  mutate(player2rank = NA)

for(i in 1:nrow(DF_test)) {
  sth = rankings2 %>%
    filter(week_title < DF_test[i,4])

  sth = sth %>%
    filter(sth[,3] == max(sth[,3]))

  sth1 = sth %>%
    filter(as.character(sth[,2]) == as.character(DF_test[i,1]))
  
  DF_test$player1rank[i] = sth1[1,4]
  
  sth2 = sth %>%
    filter(as.character(sth[,2]) == as.character(DF_test[i,2]))
  
  DF_test$player2rank[i] = sth2[1,4]
}

save(D, file = "D.RData")
