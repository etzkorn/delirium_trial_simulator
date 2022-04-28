source("competing_simulate_data.R")
source("ui.R")
source("server.R")

library(shiny)
library(tidyverse)
library(gt)

# Run the application
shinyApp(ui = ui, server = server)
