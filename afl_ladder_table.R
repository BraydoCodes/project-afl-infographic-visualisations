# this assumes that selected_team and selected_year variables have been placed in memory
# this also assumes you have run interesting_insights.R correctly

library(dplyr)
library(gt) # for table generation
library(gtExtras)



# filter by year
ladder_for_year <- ranking_for_clubs_all %>% filter(Year == selected_year)

target_team_row_num <- which(ladder_for_year$Team == selected_team)

traditional_final_positions <- c(1, 2, 3, 4, 5, 6, 7, 8)

# create a gt ladder, remove year from the table
ladder_table <- ladder_for_year %>% gt() %>% 
  cols_label(RoundPlayed = "Round Played",
             TotalPoints = "Total Points",
             position = "Position") %>% 
  gt_highlight_rows(
  rows = traditional_final_positions,
  fill = "lightgrey",
  bold_target_only = TRUE
) %>% gt_highlight_rows(
  rows = target_team_row_num,
  fill = "gold2"
) %>%
  gt_theme_pff()

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

