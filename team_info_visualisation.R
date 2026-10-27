round_graph_fill <- c("#774762FF", "#BA6E1DFF", "#D6BB3BFF", "#755028FF", "#F2DD78FF", "#205F4BFF", "#913914FF", 
                      "#585854FF", "#F0A430FF", "#768048FF", "#800000FF", "#1B3A54FF", "#774762FF", "#BA6E1DFF", 
                      "#D6BB3BFF", "#755028FF", "#F2DD78FF", "#205F4BFF", "#913914FF", "#585854FF", "#F0A430FF", 
                      "#768048FF", "#800000FF", "#1B3A54FF", "#774762FF", "#BA6E1DFF", "#D6BB3BFF", "#755028FF")

# use the stats table that lists every player that has played an afl game since 2012
yearly_team_stats <- stats_data %>% dplyr::filter(Year == selected_year)

# count competition team number - we do this because teams are likely to be added
competition_team_numbers <- unique(yearly_team_stats$Team)

# filter down stats table
stats_for_team <- yearly_team_stats %>% dplyr::filter(Team == selected_team) 


# find the experience in the team for each round of the year
avg_games_player <- stats_for_team %>% group_by(Round) %>% summarise(Avg_Experience = mean(GameNumber), Total_Experience = sum(GameNumber))

# to order, we must filter out the rounds the team did not play in the year
round_order_team <- data.frame(Round = round_order) %>% filter(Round %in% avg_games_player$Round)
avg_games_player <- avg_games_player %>% mutate(Round = factor(Round, labels = round_order_team[,1]))


# find the max difference, used to annotate
max_diff_agp <- which(abs(diff(avg_games_player$Total_Experience)) == max(abs(diff((avg_games_player$Total_Experience)))))
highest_height <- max(as.numeric(avg_games_player[max_diff_agp,]["Avg_Experience"]), as.numeric(avg_games_player[max_diff_agp+1,]["Avg_Experience"]))



# plot the avg games per player onto a bar plot
avg_games_plot <- ggplot(avg_games_player, aes(x = Round, y = Avg_Experience, fill = Round)) + geom_col( ) +
  labs(y = "Average Experience",
       title = paste0(selected_team,"'s Average Experience in the team over the whole season in ", selected_year),
       caption = "The AFL") + 
  scale_fill_manual(values = round_graph_fill) + theme_classic() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'),
        plot.caption = element_text(color = "grey40", hjust = 0.5, size = 12, margin = margin(5, 0, 0, 0))) + 
  geom_rect(aes(xmin = max_diff_agp - 0.5, xmax = max_diff_agp + 1.5,
                ymin = 0, ymax = highest_height),
            fill = "red", alpha = 0.02) +
  annotate("text", x= max_diff_agp, y=highest_height, label="Largest gap in experience", 
           vjust=-highest_height * 0.01, size=2)

avg_games_plot

# find the number of players that played a game that year
unique_players <- n_distinct(stats_for_team$PlayerName)
avg_competition_unique_players <- floor(n_distinct(yearly_team_stats$PlayerName) / length(competition_team_numbers))

# find players who played all game time
games_player_all_time <- stats_for_team %>% dplyr::filter(X.Played == 100)
num_of_gpat <- nrow(games_player_all_time)

## competition average
avg_all_games_played_per_team <- yearly_team_stats %>% dplyr::filter(X.Played == 100) %>% group_by(Team) %>% summarise(total_all_played = n())
avg_one <- mean(avg_all_games_played_per_team$total_all_played)

# keep only the information from the player that had the most disposals in each round
highest_possession_getters <- stats_for_team %>% group_by(Round) %>% dplyr::filter(Disposals == max(Disposals))
num_of_hpg <- n_distinct(highest_possession_getters$PlayerId)

# competition average most disposals
all_highest_possession_getters <- yearly_team_stats %>% group_by(Team, Round) %>% dplyr::filter(Disposals == max(Disposals))
average_hpg <- n_distinct(all_highest_possession_getters$PlayerId) / length(competition_team_numbers)

# create the dataframe , this solution right now is quite volatile
team_player_stats <- data.frame(
  team=rep(c(selected_team, "All Teams"), each = 3),
  stat=rep(c('Number All Game Time', 'Unique Highest Possession Getters', 'Unique Players'), times = 2),
  value=c(num_of_gpat, num_of_hpg, unique_players, avg_one, average_hpg, avg_competition_unique_players))

# all in one variation
team_player_plot <- ggplot(team_player_stats, aes(x=stat, y=value, fill=team)) + 
  labs(x = 'Statistic', y = 'Value', title = 'Number of in player stats for selected team & the competitions average') + 
  geom_bar(position = 'dodge', stat = 'identity') + scale_fill_manual(values = round_graph_fill) + 
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'), axis.title.x = element_blank()) 

compared_colours <- c("#999999", current_team_colour)


differences <- function(tps_df){
  sep_stat_additive <- nrow(tps_df) / 2
  diff_vec <- c()
  for(i in 1:sep_stat_additive){
    diff_vec <- append(diff_vec, round(tps_df[i,]["value"] - tps_df[i + sep_stat_additive,]["value"], 1))
  }
  unlist(diff_vec)
}
v <- data.frame(team = selected_team, stat = unique(team_player_stats$stat), y = team_player_stats[1:3,]$value, Value = differences(team_player_stats))
v$Value <- lapply(v$Value, function(x) sprintf("%+3.1f", x))

sub <- paste("The <strong>Number All Game Time</strong> refers to players who were on the ground for the entire duration of the game, without ever coming off for a break.<br> 
                        <strong>Unique Highest Possession Getter</strong> is based on the unique diversity of players who had the most possession or disposals at the end of each game.<br>
                        <strong>Unique Players</strong> refers to the number of players that took to the field in a given season.")
# spread version
team_player_plot_multi <- ggplot(team_player_stats, aes(x = team, y = value, fill = team)) + 
  labs(x = 'Statistic', y = 'Number of Occurrences', title = 'Number of in player stats for selected team & the competitions average',
       subtitle = sub, fill = "Team") + 
  geom_col( ) + theme_few() + scale_fill_manual(values = compared_colours) +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'), axis.title.x = element_blank(), plot.subtitle=element_markdown()) + 
  facet_wrap(~stat) + geom_text(data = v, mapping = aes(x = 2, y = y, label = Value),
    vjust = -0.5, fontface = "bold"
  )

team_player_plot_multi