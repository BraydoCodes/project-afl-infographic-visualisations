source('setup.R')

ui <- fluidPage( theme = bs_theme(version = 5, bootswatch = "litera"),
  fluidRow(
    column(width = 2,
      pickerInput(inputId = 'mode', label = 'Mode', choices = mode_options)
    ),
    column(width = 2,
    pickerInput(inputId = 'team', label = 'Team', choices = team_names)
    ),
    column(width = 2,
    sliderInput(inputId = 'year', label = 'Year', value = 2013, min = 2013, max = 2025)
    ),
    ),
  plotOutput(outputId = 'plot', height="900px")
)

server <- function(input, output, session) {
    source("interesting_insights.R", local = TRUE)
    source("afl_ladder_table.R", local = TRUE)
    source("stats_for_ladder.R", local = TRUE)
    source("team_info_visualisation.R", local = TRUE)
  
    # mode <- reactive({ input$mode }) can't get it to be of correct type
    # debounce to prevent it from generating graphs too quickly
    year <- reactive({ input$year }) %>% debounce(5000)
    team <- reactive({ input$team }) %>% debounce(5000)
  
    output$plot <- renderPlot(
      if (input$mode == mode_options[1]){
        create_ladder_vis(year(), team())
      } else {
        create_team_vis(year(), team())
      }
    ) %>%
      bindCache(year(), team())
}
  
shinyApp(ui = ui, server = server)