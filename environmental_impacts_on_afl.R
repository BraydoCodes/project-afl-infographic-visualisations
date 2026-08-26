# scoring based on influences of environment or location (physical factors)
library(dplyr)
games_data <- read.csv("./games.csv")

# target grouping column
target_factor = "Rainfall"

# select required columns
game_scores <- games_data %>% select(HomeTeamScore, AwayTeamScore, target_factor)

# group by influence, then summarise the scoring to total, number, average and difference
game_scores_with_influence <- games_data %>% group_by(.data[[target_factor]]) %>% 
  summarise(TotalScore = sum(HomeTeamScore,AwayTeamScore), AverageScore=mean((HomeTeamScore + AwayTeamScore)/2), 
              AvgScoreDiff = abs(mean(HomeTeamScore - AwayTeamScore)), TimesPlayed = n())