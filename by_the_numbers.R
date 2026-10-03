# this file will present a teams season summed up by numbers
# it will provide an insight to the numbers that made up the season

# this function converts a goals.points double into a integer score
convert_goal_points_to_number <- function(x){
  return(x %/% 1 * 6 + x %% 1 * 10)
}

# packages required
# library(dplyr) -> don't need because tidyverse has this at the core?
library(tidyverse)
library(ggtext)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)

# assign the font
info_font <- "Bahnschrift"

overlap_perc <- 0.01

# systemfonts::system_fonts() # - uncomment to view all downloaded fonts
# change variables here
selected_year = "2025"
selected_team = "Brisbane"

round_graph_fill <- c("#774762FF", "#BA6E1DFF", "#D6BB3BFF", "#755028FF", "#F2DD78FF", "#205F4BFF", "#913914FF", 
                      "#585854FF", "#F0A430FF", "#768048FF", "#800000FF", "#1B3A54FF", "#774762FF", "#BA6E1DFF", 
                      "#D6BB3BFF", "#755028FF", "#F2DD78FF", "#205F4BFF", "#913914FF", "#585854FF", "#F0A430FF", 
                      "#768048FF", "#800000FF", "#1B3A54FF", "#774762FF", "#BA6E1DFF", "#D6BB3BFF", "#755028FF")

# use the stats table that lists every player that has played an afl game since 2012
stats_data <- read.csv("./stats.csv")

yearly_team_stats <- stats_data %>% filter(Year==selected_year)

# count competition team number - we do this because teams are likely to be added
competition_team_numbers <- unique(yearly_team_stats$Team)

# filter down stats table
stats_for_team <- yearly_team_stats %>% filter(Team==selected_team) 

# keep the order preserved for later
round_order <- unique(stats_for_team$Round)

# find the experience in the team for each round of the year
avg_games_player <- stats_for_team %>% group_by(Round) %>% summarise(Avg_Experience = mean(GameNumber), Total_Experience = sum(GameNumber))
avg_games_player <- avg_games_player %>% mutate(Round = factor(Round, labels = round_order))
# find the max difference, used to annotate
max_diff_agp <- which(diff(avg_games_player$Total_Experience) == max(diff(avg_games_player$Total_Experience)))
highest_height <- max(as.numeric(avg_games_player[max_diff_agp,]["Avg_Experience"]),as.numeric(avg_games_player[max_diff_agp+1,]["Avg_Experience"]))


# plot the avg games per player onto a bar plot
avg_games_plot <- ggplot(avg_games_player, aes(x=Round, y=Avg_Experience, fill=Round)) + 
  geom_col( ) +
  labs(
    fill = NULL, colour = NULL, y = "Average Experience",
    title = paste0(selected_team,"'s Average Experience in the team over the whole season in ", selected_year),
    caption = "The AFL"
  ) + scale_fill_manual(values=round_graph_fill) +
  theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 15),
        plot.caption = element_text(color = "grey40", hjust = 0.5, size = 12, margin = margin(5, 0, 0, 0))) + 
  geom_rect(aes(xmin = max_diff_agp - 0.5, xmax = max_diff_agp + 1.5,
                ymin = 0, ymax = highest_height),
            fill = "red", alpha = 0.02) +
  annotate("text", 
           x= max_diff_agp, y=highest_height,
           label="Largest gap in experience", 
           vjust=-highest_height * overlap_perc,
           size=2)

avg_games_plot


# find the number of players that played a game that year
unique_players <- n_distinct(stats_for_team$PlayerName)
avg_competition_unique_players <- floor(n_distinct(yearly_team_stats$PlayerName) / length(competition_team_numbers))

# find players who played all game time
games_player_all_time <- stats_for_team %>% filter(X.Played==100)
num_of_gpat <- nrow(games_player_all_time)

## competition average
avg_all_games_played_per_team <- yearly_team_stats %>% filter(X.Played==100) %>% group_by(Team) %>% summarise(total_all_played = n())
avg_one <- mean(avg_all_games_played_per_team$total_all_played)

# keep only the information from the player that had the most disposals in each round
highest_possession_getters <- stats_for_team %>% group_by(Round) %>% filter(Disposals == max(Disposals))
num_of_hpg <- n_distinct(highest_possession_getters$PlayerId)

# competition average most disposals
all_highest_possession_getters <- yearly_team_stats %>% group_by(Team, Round) %>% filter(Disposals == max(Disposals))
average_hpg <- n_distinct(all_highest_possession_getters$PlayerId) / length(competition_team_numbers)

# create the dataframe , this solution right now is quite volatile

team_player_stats <- data.frame(
  team=rep(c(selected_team, "All Teams"), each=3),
  stat=rep(c('Number All Game Time', 'Unique Highest Possession Getters', 'Unique Players'), times=2),
  value=c(num_of_gpat, num_of_hpg, unique_players, avg_one, average_hpg, avg_competition_unique_players)
)

# all in one variation
team_player_plot <- ggplot(team_player_stats, aes(x=stat, y=value, fill=team)) + 
  labs(x='Statistic', y='Value', title='Number of in player stats for selected team & the competitions average') + 
  geom_bar(position='dodge', stat='identity') + scale_fill_manual(values=round_graph_fill) + 
  theme(plot.title = element_text(hjust=0.5, size=15, face='bold'), axis.title.x=element_blank()) 

# spread version
team_player_plot_multi <- ggplot(team_player_stats, aes(x=team, y=value, fill=value)) + 
  labs(x='Statistic', y='Value', title='Number of in player stats for selected team & the competitions average') + 
  geom_col( ) + theme_few() +
  theme(plot.title = element_text(hjust=0.5, size=15, face='bold'), axis.title.x=element_blank()) + 
  facet_wrap(~stat)

team_player_plot_multi

###### LADDER
# This next section focuses on the success of the team
games_data <- read.csv("./games.csv")
all_games_year <- games_data %>% filter(Year==selected_year)

## setting to negative 1 to remove unneeded rows in cleanup
all_games_year$score <- -1
# locate and move the score for the selected team for one column
score_games_year <- all_games_year %>% mutate(first_half_to_second_half = case_when(HomeTeam == selected_team ~ convert_goal_points_to_number( x = HomeTeamScoreHT)/HomeTeamScore, 
                                                                                    AwayTeam == selected_team ~ convert_goal_points_to_number( x = AwayTeamScoreHT)/AwayTeamScore), 
                                              score = case_when(HomeTeam == selected_team ~ HomeTeamScore, AwayTeam == selected_team ~ AwayTeamScore))
score_games_year <- score_games_year %>% select(GameId, Venue, score, first_half_to_second_half) %>%  filter(score != -1)
# insights based on the venue
venue_summary <- score_games_year %>% group_by(Venue) %>% summarise(GamesPlayed = n(), AverageScore = round(mean(score), 1))

venue_plot <- ggplot(venue_summary, aes(x = Venue, y = AverageScore, fill = GamesPlayed, label = AverageScore)) + 
  labs(x = "Venue Played", y = "Average Score", title = paste0("Average Score achieved at each venue in ", selected_year), fill = "Games Played") +
  scale_fill_continuous(palette = brewer.pal(n = 3, name = "Greens")) +
  geom_bar(stat = "identity", width = 0.6) + 
  geom_label(nudge_y = 3.75) + theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'), 
        legend.title = element_text(), legend.position = "bottom")
venue_plot



total_scores_over_100 <- score_games_year %>% count(score >= 100)
higher_first_half_score <- score_games_year %>% count(first_half_to_second_half >= 0.5) 
total_scores_over_100[2,2]
team_scores_over_100 <- paste0("Scores over 100", "Higher first half totals")
stats_descripter <- ggplot() + theme_void() + theme(panel.background = element_rect(fill = "white"), 
  plot.margin = margin_auto(1, unit = "cm"),
  plot.background = element_rect(fill = "grey30", colour = "black", linewidth = 0.5)) +
  annotate("text", x = 6, y = 25.75, size = 12, label = paste0("Summary of ", selected_team, "s' season in ", selected_year), fontface = "bold") +
  annotate("label", x = 4, y = 25, size = 8, label = "Scores over 100", fill="#774762FF") +
  annotate("label", x = 8, y = 25, size = 8, label = "Higher first half totals", fill="#205F4BFF") +
  annotate("text", x = 4, y = 24.5, size = 5, label = total_scores_over_100[2,2], fontface = "bold") +
  annotate("text", x = 8, y = 24.5, size = 5, label = higher_first_half_score[2,2], fontface = "bold") +
  annotate("text", x = 0, y = 22.5, label="") + annotate("text", x = 12, y = 27, label="")
  
stats_descripter