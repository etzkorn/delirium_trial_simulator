library(shiny)
library(tidyverse)
library(gt)
set.seed(123456)

source("competing_simulate_data.R")
source("ui.R")
source("server.R")

# Run the application
shinyApp(ui = ui, server = server)
