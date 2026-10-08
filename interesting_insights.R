# this is building the ladder - the official system that ranks the teams position every year
ladder_games_dataframe <- data.frame(Year = games_data$Year, HomeTeam = games_data$HomeTeam, AwayTeam = games_data$AwayTeam, HomeTeamScore = games_data$HomeTeamScore, AwayTeamScore = games_data$AwayTeamScore, Round = games_data$Round)

# disregard finals
finals_names <- c("Qualifying Final", "Elimination Final", "Preliminary Final", "Semi Final", "Grand Final")
ladder_games_dataframe <- ladder_games_dataframe %>% dplyr::filter(! Round %in% finals_names)

# remove round as it is unneeded  - nope
ladder_games_clean <- ladder_games_dataframe %>% dplyr::filter(Year == selected_year | Year == previous_year)

# remove away team information
home_group_games <- ladder_games_clean %>% select(-"AwayTeam")
# remove home team information
away_group_games <- ladder_games_clean %>% select(-"HomeTeam")

colnames(home_group_games) <- c("Year","Team", "ScoreFor", "ScoreAgainst", "Round")
# inverse as it is away
colnames(away_group_games) <- c("Year", "Team", "ScoreAgainst", "ScoreFor", "Round")

all_scores_overall <- rbind(home_group_games, away_group_games)

all_scores_overall$Point <- ifelse(
  ( 
    (all_scores_overall$ScoreFor > all_scores_overall$ScoreAgainst)
  ),
  4,  # if they score more then give then 4 points
  ifelse(
    ( 
      (all_scores_overall$ScoreFor == all_scores_overall$ScoreAgainst)
    ), 
    2, # then 2 points if they both got the same score
    0   # then 0 if they lost
  ))

table_for_clubs <- all_scores_overall %>% select(-"Round") %>% group_by(Team, Year) %>% summarise(RoundPlayed = n(),
                                                                             Wins = sum(Point == 4),
                                                                             Draws = sum(Point == 2),
                                                                             Loses = sum(Point == 0),
                                                                             TotalPoints = sum(Point), 
                                                                             Percentage = round(sum(ScoreFor)/sum(ScoreAgainst) * 100, 1))

# rank position for each year
ranking_for_clubs_all <- table_for_clubs %>% arrange(-TotalPoints, -Percentage) %>% group_by(Year) %>%
  mutate(position = order(order(rank(TotalPoints, ties.method = "min"),decreasing = TRUE)))

# ladder for selected year, change in setup.R
ranking_for_clubs <- ranking_for_clubs_all %>% dplyr::filter(Year == selected_year)

# previous year if applicable
previous_ranking_for_clubs <- ranking_for_clubs_all %>% dplyr::filter(Year == previous_year)

## Find trajectory of each clubs percentage in the season
all_scores_target <- all_scores_overall %>% dplyr::filter(Year == selected_year)
all_scores_target$diff <- all_scores_target$ScoreFor - all_scores_target$ScoreAgainst
all_scores_target <- all_scores_target %>% select(-"ScoreFor", -"ScoreAgainst")
all_scores_target$runningtotal <- 0


points_progression <- all_scores_target[0,]
for(i in 1:length(team_names)){
  current <- all_scores_target %>% dplyr::filter(Team == team_names[i]) %>% arrange(factor(Round, levels = round_order))
  iter = as.numeric(dplyr::count(current))
  for(i in 1:iter){
    if(i > 1){
      current[i,]$runningtotal <- as.numeric(current[i-1,]$runningtotal) + as.numeric(current[i,]$diff)
    } else {
      current[i,]$runningtotal = as.numeric(current[i,]$diff)
    }
  }
  points_progression <- rbind(points_progression, current)
}