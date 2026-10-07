# this file will present a teams season summed up by numbers
# it will provide an insight to the numbers that made up the season
# this also assumes you have run interesting_insights.R correctly

# this function converts a goals.points double into a integer score
convert_goal_points_to_number <- function(x){
  return(x %/% 1 * 6 + x %% 1 * 10)
}

# packages required
library(tidyverse)
library(ggtext)
library(ggplot2)
library(gghighlight)
library(ggthemes)
library(ggicons)
library(ggpattern)
library(RColorBrewer) # for some of the palettes used
library(patchwork) # for merging r studio graphs
library(showtext)
# the line below can be used to download ionicons (the icon service) used so that the icons package is aware of it
# icons::download_ionicons()

# SEE ISSUE #1 - issues with font [https://github.com/BraydoCodes/project-afl-infographic-visualisations/issues/1]
font_add_google("Libre Franklin")
showtext_auto() 
font_family <- "Libre Franklin"

# CHANGE YEAR AND TEAM YOU WANT TO SELECT - YEAR MUST BE BETWEEN 2012-CURRENT AND TEAM MUST BE THE LOCATION OF THE TEAM 
selected_year = "2015" # this should be in a certain file that is referenced here
selected_team = "Brisbane" # could be interesting if each team has an associated colour

round_graph_fill <- c("#774762FF", "#BA6E1DFF", "#D6BB3BFF", "#755028FF", "#F2DD78FF", "#205F4BFF", "#913914FF", 
                      "#585854FF", "#F0A430FF", "#768048FF", "#800000FF", "#1B3A54FF", "#774762FF", "#BA6E1DFF", 
                      "#D6BB3BFF", "#755028FF", "#F2DD78FF", "#205F4BFF", "#913914FF", "#585854FF", "#F0A430FF", 
                      "#768048FF", "#800000FF", "#1B3A54FF", "#774762FF", "#BA6E1DFF", "#D6BB3BFF", "#755028FF")

# use the stats table that lists every player that has played an afl game since 2012
stats_data <- read.csv("./stats.csv")

yearly_team_stats <- stats_data %>% filter(Year == selected_year)

# count competition team number - we do this because teams are likely to be added
competition_team_numbers <- unique(yearly_team_stats$Team)

# filter down stats table
stats_for_team <- yearly_team_stats %>% filter(Team == selected_team) 

# keep the order preserved for later
round_order <- unique(stats_for_team$Round)

# find the experience in the team for each round of the year
avg_games_player <- stats_for_team %>% group_by(Round) %>% summarise(Avg_Experience = mean(GameNumber), Total_Experience = sum(GameNumber))
avg_games_player <- avg_games_player %>% mutate(Round = factor(Round, labels = round_order))

# find the max difference, used to annotate
max_diff_agp <- which(diff(avg_games_player$Total_Experience) == max(diff(avg_games_player$Total_Experience)))
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
           vjust=-highest_height * overlap_perc, size=2)

# find the number of players that played a game that year
unique_players <- n_distinct(stats_for_team$PlayerName)
avg_competition_unique_players <- floor(n_distinct(yearly_team_stats$PlayerName) / length(competition_team_numbers))

# find players who played all game time
games_player_all_time <- stats_for_team %>% filter(X.Played == 100)
num_of_gpat <- nrow(games_player_all_time)

## competition average
avg_all_games_played_per_team <- yearly_team_stats %>% filter(X.Played == 100) %>% group_by(Team) %>% summarise(total_all_played = n())
avg_one <- mean(avg_all_games_played_per_team$total_all_played)

# keep only the information from the player that had the most disposals in each round
highest_possession_getters <- stats_for_team %>% group_by(Round) %>% filter(Disposals == max(Disposals))
num_of_hpg <- n_distinct(highest_possession_getters$PlayerId)

# competition average most disposals
all_highest_possession_getters <- yearly_team_stats %>% group_by(Team, Round) %>% filter(Disposals == max(Disposals))
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

# spread version
team_player_plot_multi <- ggplot(team_player_stats, aes(x = team, y = value, fill = value)) + 
  labs(x = 'Statistic', y = 'Value', title = 'Number of in player stats for selected team & the competitions average') + 
  geom_col( ) + theme_few() +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'), axis.title.x = element_blank()) + 
  facet_wrap(~stat)

###### LADDER
# This next section focuses on the success of the team
games_data <- read.csv("./games.csv")
all_games_year <- games_data %>% filter(Year==selected_year)

## setting to negative 1 to remove unneeded rows in cleanup
all_games_year$score <- -1

# locate and move the score for the selected team for one column
score_games_year <- all_games_year %>% mutate(first_half_to_second_half = case_when(HomeTeam == selected_team ~ convert_goal_points_to_number(x = HomeTeamScoreHT)/HomeTeamScore, 
                                                                                    AwayTeam == selected_team ~ convert_goal_points_to_number(x = AwayTeamScoreHT)/AwayTeamScore), 
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

# find the count of both total scores over 100, and when the selected team score more in the first half over the second half
total_scores_over_100 <- score_games_year %>% count(score >= 100)
higher_first_half_score <- score_games_year %>% count(first_half_to_second_half >= 0.5) 

# create an empty ggplot and add the text/labels as a text infographic
stats_descripter <- ggplot() + theme_void() + theme(panel.background = element_rect(fill = "white"), 
  plot.margin = margin_auto(1, unit = "cm"),
  plot.background = element_rect(colour = "black", linewidth = 0.5)) +
  annotate("text", x = 6, y = 25.75, size = 10, label = paste0("Summary of ", selected_team, "s' season in ", selected_year), fontface = "bold") +
  annotate("label", x = 4, y = 25, size = 7, label = "Scores over 100", fill="#774762FF") +
  annotate("label", x = 8, y = 25, size = 7, label = "Higher first half totals", fill="#205F4BFF") +
  annotate("text", x = 4, y = 24.5, size = 4, label = total_scores_over_100[2,2], fontface = "bold") +
  annotate("text", x = 8, y = 24.5, size = 4, label = higher_first_half_score[2,2], fontface = "bold") +
  annotate("text", x = 0, y = 22.5, label="") + annotate("text", x = 12, y = 27, label = "")

# small graph to have alongside the original ladder table
past_comparison_of_ranking <- rbind(previous_ranking_for_clubs %>% filter(Team == selected_team),
                                    ranking_for_clubs %>% filter(Team == selected_team))
key_comparison_ranking <- past_comparison_of_ranking %>% select(c('Year', 'Wins', "Loses", 'Percentage', 'position'))
key_comparison_ranking_longer <- key_comparison_ranking %>% pivot_longer(!Year, names_to = 'Stat', values_to = 'Total')

num_of_stats <- 4

adjust <- function(p1, p2, type, inverse = FALSE){
  # MUST FOLLOW FORMAT (SIZE = 3): c(IMPROVEMENT_VALUE, DETERIORATION_VALUE, EQUAL_VALUE)
  option_icon <- c(icons::ionicons$"arrow-up-outline", icons::ionicons$"arrow-down-outline", icons::ionicons$"reorder-two-outline")
  option_pattern <- c('#639754', '#D61F1F', '#E8BF0F')
  option_fill <- c('#7BB662', '#E03C32', '#FFD301')
  df <- data.frame(option_icon, option_pattern, option_fill)
  type_index <- which(c('icon', 'pattern','fill') == type)
  filt_df <- df[,type_index]
  
  # flips variables if inverse, do not do this with the icon however
  if(inverse == TRUE && type_index != 1){
    temp_var = p1
    p1 = p2
    p2 = temp_var
  }
  
  if(p1 < p2){
    filt_df[1]
  } else if(p1 > p2){
    filt_df[2]
  } else {
    filt_df[3]
  }
}

# HELPER FUNCTION BASED ON DIFFERENCE, USED WHEN A DETERMINED DIRECTION IS REQUIRE TO MOVE OBJECT
difference_to_unit <- function(num1, num2){
  if(num1 - num2 != 0){ # we have to check this to ensure we do not divide by 0
    as.numeric((num2 - num1)/abs(num2 - num1) * (abs(num1) / 15))
  }
  else {
    0
  }
}

# FUNCTION TO PLOT A COMPARISON BETWEEN THE CURRENT AND PREVIOUS YEAR FOR A STAT, A FUNCTION IS USED DYNAMICALLY
# USES ADJUST FUNCTION AND DIFFERENCE TO UNIT FUNCTION
stat_comparison_plot <- function(d, stat, stat_name, inverse = FALSE){
  ggplot(d, aes(x = factor(Year), y = Total, fill = c("gray"))) +
    labs(x = 'Year', y = 'Total', 
         title = toupper(paste0(selected_team, "'s ", selected_year, " vs ", previous_year, " on ", stat_name)),
         subtitle = paste0("Only the ", selected_year, " Home & Away season is included.")) + 
    geom_text(aes(x = factor(Year), y = Total + (Total * 0.1), label = Total), 
              size = 10,
              fontface = "bold", family = font_family) + 
    geom_col_pattern(
      pattern_alpha = 0.1,
      pattern_fill = adjust(d[1,]["Total"], d[2,]["Total"], 'pattern', inverse),
      pattern_colour  = adjust(d[1,]["Total"], d[2,]["Total"], 'pattern', inverse),
      pattern_size = 1,
      pattern_angle = 15
    ) + gghighlight(Year == selected_year) + scale_x_discrete() +
    scale_fill_manual(values = adjust(d[1,]["Total"], d[2,]["Total"], 'fill', inverse)) + theme_few() +
    theme(text = element_text(family = font_family),
      plot.title = element_text(hjust = 0.5, size = 15, face = 'bold')) + guides(fill = "none") + 
    annotation_icon(icon = adjust(d[1,]["Total"], d[2,]["Total"], 'icon', inverse), x = selected_year , 
                    y = as.numeric(d[2,]["Total"]) / 2 + difference_to_unit(d[1,]["Total"], d[2,]["Total"]), size = 20) +
    annotate("text", x = selected_year, y = as.numeric(d[2,]["Total"]) / 2 - difference_to_unit(d[1,]["Total"], d[2,]["Total"]), 
             label = round(abs(as.numeric(d[2,]["Total"]) - as.numeric(d[1,]["Total"])),2), size = 15) + 
    theme_minimal(base_family = font_family)
}

# combine only the win stat info and plot
win_ranking <- rbind(key_comparison_ranking_longer[1,], key_comparison_ranking_longer[(1 + num_of_stats),])
p1 <- stat_comparison_plot(win_ranking, "Total", "Number of Wins")
# combine only the percentage stat info and plot
perc_ranking <- rbind(key_comparison_ranking_longer[3,], key_comparison_ranking_longer[(3 + num_of_stats),])
p2 <- stat_comparison_plot(perc_ranking, "Total", "Percentage at End of Season")

loss_ranking <- rbind(key_comparison_ranking_longer[2,], key_comparison_ranking_longer[(2 + num_of_stats),])
p3 <- stat_comparison_plot(loss_ranking, "Total", "Number of Losses", TRUE)

position_ranking <- rbind(key_comparison_ranking_longer[4,], key_comparison_ranking_longer[(4 + num_of_stats),])
p4 <- stat_comparison_plot(position_ranking, "Total", "Ladder Position at End of Season", TRUE)

# seemingly cannot get the table to extend downwards
((p1 / p3) | (p2 / p4)) + ladder_table

showtext_auto(FALSE) 

# TODO 
# LOSSES AND POSITION ARE INVERT IN FUNCTION RECOLOUR

# finally put all the graphs together
vis <- stats_descripter + venue_plot + team_player_plot_multi + avg_games_plot

# add an annotation to the top of the plots
vis_location <- "current_vis_year_team.png"
test <- ggsave(filename = vis_location, plot = vis, width = 50, height = 50/(1920/1080), units = "cm") # this currently assumes you have a 1980 by 1080 monitor as it resizes to 1920 by 1080 p