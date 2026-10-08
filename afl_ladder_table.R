# this assumes that selected_team and selected_year variables have been placed in memory
# this also assumes you have run interesting_insights.R correctly

# filter by year
ladder_for_year <- ranking_for_clubs_all %>% dplyr::filter(Year == selected_year)

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
    google_font(name = font_family),
    default_fonts()), size = 10) %>% data_color(columns = Team, target_columns = colour, palette = palette_order) %>% 
  data_color(columns = Team, target_columns = Team, alpha = 0.2, autocolor_text = FALSE, palette = palette_order)  # to ensure it all fits

ladder_table
