# on a target player and stat
# determine the percentile (of how their performance in the stat compares to others) that they sit in

library(dplyr)

# use the stats table that lists every player that has played an afl game since 2012
stats_data <- read.csv("./stats.csv")

target_player <- "Patrick Dangerfield"
target_players <- c("Max Gawn", "Brodie Grundy", "Lloyd Meek", "Darcy Cameron")

# This variable's spelling must be align to what is in the stats_data table. - requires ""
target_stat <- "HitOuts"

# select only required columns in dataframe
stats_with_single_stat <- select(stats_data, PlayerName, target_stat)

# grab the average of the stat per game, could be altered to total in a future iteration
grouped_stats_with_single_stat <- stats_with_single_stat %>% group_by(PlayerName) %>% summarise(Average = mean(.data[[target_stat]]))

# rounding and clean up
# remove players that have not recorded a stat
grouped_stats_with_single_stat <- grouped_stats_with_single_stat %>% filter(Average > 0)
grouped_stats_with_single_stat$Average = round(grouped_stats_with_single_stat$Average,digits = 1)

# calculate the percentage where the player is the top [percent rank] for that particular stat
rank_for_stat <- grouped_stats_with_single_stat %>% mutate(Percentage = 100 - (rank(Average)/length(Average)) * 100)

# filter down to the selected targeted player (for one)
# rank_for_target_player <- rank_for_stat %>% filter(PlayerName == target_player)
# or for many
rank_for_target_players <- rank_for_stat %>% filter(PlayerName %in% target_players)
