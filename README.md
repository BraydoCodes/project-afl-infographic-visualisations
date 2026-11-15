# project-afl-infographic-visualisations
A project that is a continuation of some of my previous university work and where I discover my love for data vis. This is about infographic/graph filled posters creating in RStudio and using credited AFL data sources.

The datasets I worked with in this repository include:
- AFL Dataset sourced on Kaggle, credit to [].

## SETUP
I recommend that if you plan to work with these scripts, to use [RStudio](https://posit.co/downloads) to launch and run this project.

Run the `shiny.R` script located in the directory, it will load the necessary packages and global variables in `setup.R`. Currently, it is required to download the `ionicons` for icons used in these visualisations, this is only required once and can be removed or commented out. 

Check out `team_info_visualisation` variable called `stats_row_names` for all the stats you can show in one of the team visualisations, you can add stats to the `valid_stats` vector, if this is done incorrectly it will stop the program.

### App Navigation
When running the shiny script, you will have the options to switch the mode, team and year in the same runtime. You must give it a bit of time for each action, a successful action is denoted when the screen is grayed out like so:
  
<img width="1918" height="995" alt="image" src="https://github.com/user-attachments/assets/524d99d9-9812-42fc-b68a-56eab5f492aa" />
The mode is able to be navigated as either `TeamSummary` or `YearSummary` in this current build.

- See below


           
<img width="309" height="155" alt="image" src="https://github.com/user-attachments/assets/665af193-cc22-4f60-838e-14ed20793dd8" />

The team dropdown also allows you to select from 18 teams, where the year is from 2013-2025, there may be plans to add 2026 data soon, once it is verified:
    
<img width="310" height="663" alt="image" src="https://github.com/user-attachments/assets/26443a91-ad4d-4a9e-afc2-78cbfa6c37b3" />
<img width="326" height="98" alt="image" src="https://github.com/user-attachments/assets/a8ed1eef-d8d8-4d42-b746-191578fc97cb" />





## Old Version

To view the visualisation and create your own personalised visualisation, use the `setup.R` file to run the whole project, you can add your own data analysis files `.R` files. You will find that there are multiple modes, you can toggle them by adjust the `current_mode` variable to any string that is contained within the `mode_options` vector variable.

There are plans to turn this into a R Shiny Application, however editting your `setup.R` file will allow you to customise the Year and Team that the visualisation focuses on, ensure that you read the comments on the script to ensure that they are valid.

## CURRENT TODO LIST
[x] -  Ladder position and facts visualisation

[x] - Team player insights for a given year (both team and individual)

[x] - Shiny application with player, year and team selection dropdowns 

[ ] - Select a player and the stats of the player are shown, highlight their biggests strength and most suprising statistic/s
  - Currently not in planned scope
