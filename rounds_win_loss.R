# packages required
library(dplyr)

# this file can target a specific club, and present their win loss record
games_data <- read.csv("./games.csv")

# this is building the required columns for a win loss
ladder_games_dataframe <- data.frame(Year = games_data$Year, HomeTeam = games_data$HomeTeam, AwayTeam = games_data$AwayTeam, HomeTeamScore = games_data$HomeTeamScore, AwayTeamScore = games_data$AwayTeamScore, Round = games_data$Round)

# disregard finals
finals_names <- c("Qualifying Final", "Elimination Final", "Preliminary Final", "Semi Final", "Grand Final")
ladder_games_dataframe <- ladder_games_dataframe %>% filter(! Round %in% finals_names)

# remove round as it is unneeded
ladder_games_clean <- ladder_games_dataframe %>% select(-"Round")

# Target club
target_club <- "Adelaide"

# filter games to only those involving the selected team
home_games <- ladder_games_clean[ladder_games_dataframe$HomeTeam == target_club,]
away_games <- ladder_games_clean[ladder_games_dataframe$AwayTeam == target_club,]

# remove away team information
home_games <- home_games %>% select(-"AwayTeam")
# remove home team information
away_games <- away_games %>% select(-"HomeTeam")

colnames(home_games) <- c("Year","Team", "ScoreFor", "ScoreAgainst")
# inverse as it is away
colnames(away_games) <- c("Year", "Team", "ScoreAgainst", "ScoreFor")

team_scores <- rbind(home_games, away_games)

team_scores$WinLoss <- ifelse(
  ( 
    (team_scores$ScoreFor > team_scores$ScoreAgainst)
  ),
  "W",  # if they score more they 'W'
  ifelse(
    ( 
      (team_scores$ScoreFor == team_scores$ScoreAgainst)
    ), 
    "D", # then 'D' if they both got the same score
    "L"   # then 'L' if they lost
  ))

win_loss <- team_scores %>% group_by(Team, Year) %>% summarise(Wins = sum(WinLoss == "W"), Drawn = sum(WinLoss == "D"), Losses =sum(WinLoss == "L"))