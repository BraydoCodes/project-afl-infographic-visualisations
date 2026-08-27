# this file will present a teams season summed up by numbers
# it will provide an insight to the numbers that made up the season

# packages required
library(dplyr)

# change variables here
selected_year = "2025"
selected_team = "Adelaide"

# use the stats table that lists every player that has played an afl game since 2012
stats_data <- read.csv("./stats.csv")

# filter down stats table
stats_for_team <- stats_data %>% filter(Year==selected_year, Team==selected_team) 

# find the experience in the team for each round of the year
avg_games_player <- stats_for_team %>% group_by(Round) %>% summarise(Avg_Experience = mean(GameNumber), Total_Experience = sum(GameNumber))

# find the number of players that played a game that year
unique_players <- n_distinct(stats_for_team$PlayerName)
unique_players

# find players who played all game time
games_player_all_time <- stats_for_team %>% filter(X.Played==100)

# keep only the information from the player that had the most disposals in each round
highest_possession_getters <- stats_for_team %>% group_by(Round) %>% filter(Disposals == max(Disposals))