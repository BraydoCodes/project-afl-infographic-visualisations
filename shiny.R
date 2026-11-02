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



server <- function(input, output, session) { source("team_info_visualisation.R", local = TRUE)
  output$plot <- renderPlot(
    team_info_vis
  )
}


shinyApp(ui = ui, server = server)
