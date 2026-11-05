# clear all variables for a clean slate
rm(list = ls())

# set the working directory to ensure that we can access the relative path of files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
# packages required for the project to work
source('library_setup.R')

# the line below can be used to download ionicons (the icon service) used so that the icons package is aware of it
# I would download this package onetime (https://search.r-project.org/CRAN/refmans/onetime/html/onetime.html), 
# to run this code only once however, I would like to keep this project as lightweight as possible (even though it is pretty intensive),
# comment out if you would like
# icons::download_ionicons()

# data sources
games_data <- read.csv("./games.csv")
players_data <- read.csv("./players.csv")
stats_data <- read.csv("./stats.csv")

# SEE ISSUE #1 - issues with font [https://github.com/BraydoCodes/project-afl-infographic-visualisations/issues/1]
font_family <- "Carter One"
font_add_google(font_family)
showtext_auto() 

mode_options <- c("YearSummary", "TeamSummary")

# change this to change the resulting output type from available options in mode_options
current_mode <- "TeamSummary"
stopifnot(current_mode %in% mode_options)

# SELECTED TEAM AND YEAR
# CHANGE YEAR AND TEAM YOU WANT TO SELECT - YEAR MUST BE BETWEEN 2012-CURRENT AND TEAM MUST BE THE LOCATION OF THE TEAM 
target_year <- 2024
target_team <- "Adelaide"


# team name and colour references
team_names <- c("Adelaide", "Brisbane", "Carlton", "Collingwood", "Essendon", "Fremantle", "Geelong", "Gold Coast",
                  "Greater Western Sydney", "Hawthorn", "Melbourne", "North Melbourne", "Port Adelaide", "Richmond", 
                  "St Kilda", "Sydney", "West Coast", "Western Bulldogs")
team_colours <- c("#004B8D", "#A30046", "#031A29", "#000000", "#CC2031", "#2A0D54", "#002B5C", "#E02112", "#F47920",
                  "#4D2004", "#0F1131", "#1A3B8E", "#008AAB", "#FFD200", "#ED1B2F", "#E1251B", "#F2A900", "#20539D" )

generate_globals <<- function(year, team){
  current_team_colour <<- team_colours[which(team_names==team)]
  
    
  # do not proceed if invalid parameters
  stopifnot((team %in% team_names && year >= 2012 && year <= 2025))
  # keep the order preserved for later
  current_year_format <<- stats_data %>% dplyr::filter(Year == year)
  year_game_data <<- games_data %>% dplyr::filter(Year == year)
  round_order <<- unique(year_game_data$Round)
}
generate_globals(target_year, target_team)


team_df_ref <<- data.frame(team_names, team_colours)
colnames(team_df_ref) <- c("Team", "Colours")

# load functions
  source("interesting_insights.R")
  source("afl_ladder_table.R")
  source("stats_for_ladder.R")
  create_ladder_vis(target_year, target_team)

  source("team_info_visualisation.R")
  create_team_vis(target_year, target_team)
