library(shiny)
library(shinyWidgets)
source('setup.R')

ui <- fluidPage(
  dropdown(
    pickerInput(inputId = 'mode', label = 'Mode', choices = mode_options),
    pickerInput(inputId = 'team', label = 'Team', choices = team_names),
    sliderInput(inputId = 'year', label = 'Year', value = 2013, min = 2012, max = 2025)),
    style = "unite", icon = icon("gear"),
    status = "danger", width = "300px",
    animate = animateOptions(
      enter = animations$fading_entrances$fadeInLeftBig,
      exit = animations$fading_exits$fadeOutRightBig
    ),
  plotOutput(outputId = 'plot')
)

server <- function(input, output, session) {
    #selected_team <- req(reactive({input$team})) # not compatible type, I could make a funcion that returns filtered datasets? function seems to be the only way that might work here
    #selected_year <- req(reactive(as.numeric({input$year})))
    source("interesting_insights.R", local = TRUE)
    source("afl_ladder_table.R", local = TRUE)
    source("stats_for_ladder.R", local = TRUE)
    source("team_info_visualisation.R", local = TRUE)
  
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