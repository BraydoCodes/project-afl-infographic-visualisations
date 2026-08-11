# packages required
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # this is to ensure that relative path works

# uncomment when necessary
games_data <- read.csv("./games.csv")
# players_data <- read.csv("./players.csv")
# stats_data <- read.csv("./stats.csv")


# this is building the ladder - the official system that ranks the teams position every year
ladder_games_dataframe <- data.frame(Year = games_data$Year, HomeTeam = games_data$HomeTeam, AwayTeam = games_data$AwayTeam, HomeTeamScore = games_data$HomeTeamScore, AwayTeamScore = games_data$AwayTeamScore, Round = games_data$Round)

# disregard finals
finals_names <- c("Qualifying Final", "Elimination Final", "Preliminary Final", "Semi Final", "Grand Final")
ladder_games_dataframe <- ladder_games_dataframe %>% filter(! Round %in% finals_names)

# remove round as it is unneeded
ladder_games_clean <- ladder_games_dataframe %>% select(-"Round")

# remove away team information
home_group_games <- ladder_games_clean %>% select(-"AwayTeam")
# remove home team information
away_group_games <- ladder_games_clean %>% select(-"HomeTeam")

colnames(home_group_games) <- c("Year","Team", "ScoreFor", "ScoreAgainst")
# inverse as it is away
colnames(away_group_games) <- c("Year", "Team", "ScoreAgainst", "ScoreFor")

all_scores_in_year <- rbind(home_group_games, away_group_games)

all_scores_in_year$Point <- ifelse(
  ( 
    (all_scores_in_year$ScoreFor > all_scores_in_year$ScoreAgainst)
  ),
  4,  # if they score more then give then 4 points
  ifelse(
    ( 
      (all_scores_in_year$ScoreFor == all_scores_in_year$ScoreAgainst)
    ), 
    2, # then 2 points if they both got the same score
    0   # then 0 if they lost
  ))

table_for_clubs <- all_scores_in_year %>% group_by(Team, Year) %>% summarise(TotalPoints = sum(Point), Percentage = sum(ScoreFor)/sum(ScoreAgainst) * 100)

# rank position for each year
ranking_for_clubs <- table_for_clubs %>% arrange(-TotalPoints, -Percentage) %>% group_by(Year) %>%
  mutate(position = order(order(rank(TotalPoints, ties.method = "min"),decreasing = TRUE)))

# ladder for target year
target_year = 2025
ranking_for_clubs <- ranking_for_clubs %>% filter(Year == target_year)
# clean up the percentage
ranking_for_clubs$Percentage <- round(ranking_for_clubs$Percentage, 1)