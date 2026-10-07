# this assumes that selected_team and selected_year variables have been placed in memory
# this also assumes you have run interesting_insights.R correctly

library(dplyr)
library(gt) # for table generation
library(gtExtras)

font_add_google("Libre Franklin")
showtext_auto() 
font_family <- "Libre Franklin"

team_names <- c("Adelaide", "Brisbane", "Carlton", "Collingwood", "Essendon", "Fremantle", "Geelong", "Gold Coast",
                "Greater Western Sydney", "Hawthorn", "Melbourne", "North Melbourne", "Port Adelaide", "Richmond", 
                "St Kilda", "Sydney", "West Coast", "Western Bulldogs")
team_colours <- c("#004B8D", "#A30046", "#031A29", "#000000", "#CC2031", "#2A0D54", "#002B5C", "#E02112", "#F47920",
                  "#4D2004", "#0F1131", "#1A3B8E", "#008AAB", "#FFD200", "#ED1B2F", "#E1251B", "#F2A900", "#20539D" )
team_df_ref <- data.frame(team_names, team_colours)
colnames(team_df_ref) <- c("Team", "Colours")

# filter by year
ladder_for_year <- ranking_for_clubs_all %>% filter(Year == selected_year)

order_colours <- ladder_for_year %>% merge(y = team_df_ref, by="Team") %>% arrange(position = factor(position, levels = position))
palette_order <- as.character(order_colours[, "Colours"])

target_team_row_num <- which(ladder_for_year$Team == selected_team)

traditional_final_positions <- c(1, 2, 3, 4, 5, 6, 7, 8)

# create a gt ladder, remove year from the table
ladder_table <- ladder_for_year %>% mutate(colour = "", .before = Team) %>% gt() %>% 
  cols_label(RoundPlayed = "Round Played",
             TotalPoints = "Total Points",
             position = "Position", colour = "|")%>% 
  gt_highlight_rows(
  rows = traditional_final_positions,
  fill = "lightgrey",
  bold_target_only = TRUE
) %>% gt_highlight_rows(
  rows = (Team == selected_team),
  fill = "gold2"
) %>%
  opt_table_font(font = c(
    google_font(name = "Libre Franklin"),
    default_fonts()), size = 10) %>% data_color(columns = Team, target_columns = colour, palette = palette_order) %>% 
  data_color(columns = Team, target_columns = Team, alpha = 0.2, autocolor_text = FALSE, palette = palette_order)  # to ensure it all fits

ladder_table
class(ladder_table) # debugging

find.package("knitr")

# this code is directly from gtextra github, removing problematic line, credit to https://github.com/jthomasmock/gtExtras/blob/master/R/gt_reprex_image.R
# create temp file
img_out <- tempfile(fileext = ".png")

# save image to temp
save_obj <- gt::gtsave(ladder_table, img_out) %>%
  utils::capture.output(type = "message") %>%
  invisible()

print(save_obj)

table_image_location <- knitr::include_graphics(img_out)[1]

