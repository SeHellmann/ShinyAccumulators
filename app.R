install.packages(setdiff(c("shiny", "dplyr", "tidyr"), installed.packages()))
library(shiny)
library(tidyr)
library(dplyr)

source("ui.r")
source("server.r")
Interactive2DSD <- shinyApp(ui, server) 
runApp(Interactive2DSD)
