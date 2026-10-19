# this file will present a teams season summed up by numbers
# it will provide an insight to the numbers that made up the season
# this also assumes you have run interesting_insights.R correctly

# this function converts a goals.points double into a integer score
convert_goal_points_to_number <- function(x){
  return(x %/% 1 * 6 + x %% 1 * 10)
}

###### LADDER
# This next section focuses on the success of the team
all_games_year <- games_data %>% dplyr::filter(Year==selected_year)

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
past_comparison_of_ranking <- rbind(previous_ranking_for_clubs %>% dplyr::filter(Team == selected_team),
                                    ranking_for_clubs %>% dplyr::filter(Team == selected_team))
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
              size = 5,
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
    annotation_icon(icon = adjust(d[1,]["Total"], d[2,]["Total"], 'icon', inverse), x = factor(selected_year) , 
                    y = as.numeric(d[2,]["Total"]) / 2 + difference_to_unit(d[1,]["Total"], d[2,]["Total"]), size = 15) +
    annotate("text", x = factor(selected_year), y = as.numeric(d[2,]["Total"]) / 2 - difference_to_unit(d[1,]["Total"], d[2,]["Total"]), 
             label = round(abs(as.numeric(d[2,]["Total"]) - as.numeric(d[1,]["Total"])),2), size = 10) + 
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

showtext_auto(FALSE) 

points_progression$diff_fill = case_when(
  points_progression$diff > 0 ~ "0",
  points_progression$diff == 0 ~ "2",
  points_progression$diff < 0 ~ "1"
)

# GRAPH based on points diff progression throughout the season
progression_plot <- ggplot(points_progression) +
  geom_line(aes(x = factor(Round, levels = round_order), y = runningtotal, group = Team)) + 
  labs(x = 'Round', y = 'Scored For - Scored Against', 
       title = toupper(paste0(selected_team, "'s year ", selected_year, " on percentage." )),
       subtitle = paste0("Only the ", selected_year, " Home & Away season is included.")) +
  gghighlight(Team == selected_team, use_direct_label = FALSE) + geom_label(aes(x = factor(Round, levels = round_order), y = runningtotal, label = runningtotal, fill=diff_fill),
                                                      size = 3.2) + scale_fill_manual(values = c("#639754", "#D61F1F", "#FFD301"))+
  theme_minimal(base_family = font_family) + theme(legend.position = "none")

print((((p1 / p3) | (p2 / p4)) + ladder_table) / progression_plot)