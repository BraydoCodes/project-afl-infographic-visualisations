# project-afl-infographic-visualisations
A project that is a continuation of some of my previous university work and where I discover my love for data vis. This is about infographic/graph filled posters creating in RStudio and using credited AFL data sources.

The datasets I worked with in this repository include:
- AFL Dataset sourced on Kaggle, credit to [].

## SETUP
I recommend that if you plan to work with these scripts, to use [RStudio](https://posit.co/downloads) to launch and run this project.

To view the visualisation and create your own personalised visualisation, use the `setup.R` file to run the whole project, you can add your own data analysis files `.R` files. You will find that there are multiple modes, you can toggle them by adjust the `current_mode` variable to any string that is contained within the `mode_options` vector variable.

There are plans to turn this into a R Shiny Application, however editting your `setup.R` file will allow you to customise the Year and Team that the visualisation focuses on, ensure that you read the comments on the script to ensure that they are valid.

## CURRENT TODO LIST
[x] -  Ladder position and facts visualisation

[x] - Team player insights for a given year (both team and individual)

[ ] - Select a player and the stats of the player are shown, highlight their biggests strength and most suprising statistic/s

[ ] - Shiny application with player, year and team selection dropdowns
