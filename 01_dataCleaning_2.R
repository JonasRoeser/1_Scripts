rm(list = ls())
load("../Roeser, Jonas - 2_Data/data1991_2017.RData")
load("../2_Data/data1991_2017.RData")

sapply(tournaments, class) # tourney_year_id "factor", tourney_dates "factor"
sapply(matchScores, class) # match_id "factor, tourney_year_id "factor"
sapply(matchStats, class) # match_id "factor"
sapply(rankings, class) # week_title "factor", player_id "factor"
sapply(playerOverviews, class) # player_id "factor", 
# winner always first, loser second

merged = merge(matchScores, matchStats, by = "match_id") # --> tourney order does not mtach!?
merged = subset(merged, select = -c(5,24,26))
