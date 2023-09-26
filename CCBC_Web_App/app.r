# Load required libraries
library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(DBI)
library(DT)

# Source Functions
source("scripts/html_functions.R")
source("scripts/sql_functions.R")

# Pull in Data (This is probably inefficient if it is slow work here)
team_batting_df <- query_database(team_batting_query)
team_pitching_df <- query_database(team_pitching_query)
player_batting_df <- query_database(player_batting_query)
player_pitching_df <- query_database(player_pitching_query)
standings_df <- query_database(standings_query)

# Define UI for the app
ui <- fluidPage(
    # Theme
    # theme = shinytheme("slate"),
    # themeSelector(),
    
    # Custom CSS
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    # Original (And Goal Code)
    
    # NavBar
    # navbarPage(title = "PBA Analytics",
    # 
    #     # TabPanels
    #     tabPanel("Player Stats", value = "player_stats",
    #             create_page("Player Stats", data_table = "table")
    #     ),
    #     tabPanel("Team Stats", value = "team_stats",
    #              create_page("Team Stats", data_table = "table")
    #     ),
    #     tabPanel("Standings", value = "standings",
    #              create_page("Standings", data_table = "table", standings = TRUE)
    #     ),
    # )
    
    # Basic DT View
    # basicPage(
    #     h2("The mtcars data"),
    #     DT::dataTableOutput("table")
    # )
    
    # Tester
    
    # Create a Shiny dashboard page
    dashboardPage(
        
        # Navbar: Set the title from the function argument
        dashboardHeader(title = "title"),
        
        # Sidebar: Add input controls
        dashboardSidebar(
            sidebarMenu(id = "sidebar_menu", # Sidebar ID for potential UI control
                        # Add dropdowns for year, season
                        selectInput(inputId = "year", label = "Select Year", choices = c("2021", "2020")),
                        selectInput(inputId = "season", label = "Select Season", choices = c("Spring", "Summer")),
                        selectInput(inputId = "team", label = "Select Team", choices = c("PBA", "OC", "VIU"))
                        
            )
        ),
        
        # Main Panel: Add content boxes
        dashboardBody(
            # Row 1: Table Box
            fluidRow(
                # DT::dataTableOutput(outputId = "table"),
                box(
                    # See Data Table
                    DT::dataTableOutput(outputId = "table"),
                    title = "Table",
                    width = 12, # Width set to maximum (Bootstrap grid is out of 12)
                    height = 400, # Set height
                )
            ),
            # Row 2: Plot Boxes
            fluidRow(
                box(
                    title = "Plot 1", # Box title
                    plotOutput("plot1") # Output ID for the first plot
                ),
                box(
                    title = "Plot 2", # Box title
                    plotOutput("plot2") # Output ID for the second plot
                )
            )
        )
    )
)

# Define server logic for the app
server <- function(input, output, session) {
    # Placeholder for server logic
    observe({
        current_tab <- input$my_tabs
    })
    
    # See what tab the user is on
    datasetInput <- reactive({
        switch(input$category,
               "Hitting" = player_batting_df,
               "Pitching" = player_pitching_df)
    })
    
    # Show the first "n" observations ----
    # The output$view depends on both the databaseInput reactive
    # expression and input$obs, so it will be re-executed whenever
    # input$dataset or input$obs is changed
    # output$table <- renderTable({
    #     head(datasetInput(), n = 20)
    # })
    
    # output$table <- renderDataTable(player_batting_df) # input$category ?
    
    # Create the output table
    output$table <- DT::renderDataTable({ player_batting_df })
}

# Run the app
shinyApp(ui, server)
