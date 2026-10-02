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

# assign the font
info_font <- "Bahnschrift"

overlap_perc <- 0.01

# systemfonts::system_fonts() # - uncomment to view all downloaded fonts
# change variables here
selected_year = "2025"
selected_team = "Adelaide"

round_graph_fill <- c("#774762FF", "#BA6E1DFF", "#D6BB3BFF", "#755028FF", "#F2DD78FF", "#205F4BFF", "#913914FF", 
                      "#585854FF", "#F0A430FF", "#768048FF", "#800000FF", "#1B3A54FF", "#774762FF", "#BA6E1DFF", 
                      "#D6BB3BFF", "#755028FF", "#F2DD78FF", "#205F4BFF", "#913914FF", "#585854FF", "#F0A430FF", 
                      "#768048FF", "#800000FF", "#1B3A54FF", "#774762FF", "#BA6E1DFF", "#D6BB3BFF", "#755028FF")

# use the stats table that lists every player that has played an afl game since 2012
stats_data <- read.csv("./stats.csv")

# filter down stats table
stats_for_team <- stats_data %>% filter(Year==selected_year, Team==selected_team) 

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
unique_players

# find players who played all game time
games_player_all_time <- stats_for_team %>% filter(X.Played==100)

# keep only the information from the player that had the most disposals in each round
highest_possession_getters <- stats_for_team %>% group_by(Round) %>% filter(Disposals == max(Disposals))

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
venue_summary <- score_games_year %>% group_by(Venue) %>% summarise(GamesPlayed = n(), AverageScore = mean(score))

total_scores_over_100 <- score_games_year %>% count(score >= 100)
higher_first_half_score <- score_games_year %>% count(first_half_to_second_half >= 0.5) 