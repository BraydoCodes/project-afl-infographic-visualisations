# this file controls required package management, it is separated to keep things clean
required_packages <- c("dplyr", "tidyverse", 
                       "ggtext", "ggplot2", "gghighlight", "ggthemes", "ggicons", "ggpattern", "ggrepel", "patchwork",
                       "RColorBrewer", "showtext", 
                       "gt", "gtExtras",
                       "shiny", "bslib", "shinyWidgets")

for(p in required_packages){
  print(paste0("Setting up ", p))
  library(p,character.only = TRUE)
}