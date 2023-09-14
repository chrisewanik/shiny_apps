# Load required libraries
library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)

# Source Functions
source("scripts/html_functions.R")

# Define UI for the app
ui <- fluidPage(
    # Theme
    theme = shinytheme("slate"),
    # themeSelector(),
    
    # Custom CSS
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    navbarPage(title = "PBA Analytics",
               tabPanel("Player Stats",
                create_page("Player Stats")
        ),
        tabPanel("Team Stats",
                 create_page("Team Stats")
        ),
    )
)

# Define server logic for the app
server <- function(input, output, session) {
    # Placeholder for server logic
}

# Run the app
shinyApp(ui, server)
