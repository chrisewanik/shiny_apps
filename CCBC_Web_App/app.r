
# Load Libraries ----------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(DBI)
library(DT)


# Source Functions --------------------------------------------------------


source("scripts/html_functions.R")
source("scripts/sql_functions.R")


# Query DB ----------------------------------------------------------------

team_batting_df <- query_database(team_batting_query)
team_pitching_df <- query_database(team_pitching_query)
player_batting_df <- query_database(player_batting_query)
player_pitching_df <- query_database(player_pitching_query)
standings_df <- query_database(standings_query)


# Dashboard Header --------------------------------------------------------


header <- dashboardHeader(title = "PBA Scouting Portal")



# Dashboard Sidebar -------------------------------------------------------


sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Player Stats", tabName = "players_tab", icon = icon("baseball")),
        menuItem("Team Stats", tabName = "teams_tab", icon = icon("baseball")),
        menuItem("Standings", tabName = "standings_tab", icon = icon("baseball")),
        selectInput(inputId = "year", label = "Select Year", choices = c("2021", "2020")),
        selectInput(inputId = "season", label = "Select Season", choices = c("Spring", "Summer")),
        selectInput(inputId = "team", label = "Select Team", choices = c("PBA", "OC", "VIU")),
        selectInput(inputId = "category", label = "Stat Category", choices = c("Hitting", "Pitching"))
    )
)


# Dashboard Body ----------------------------------------------------------


body <- dashboardBody(
    tabItems(
        # 1. Player Stats Tab ----
        tabItem(tabName = "players_tab",
                h2("Player Stats Dashboard"),
                # 1.1 Stats Table ----
                fluidRow(
                    box(
                        # See Data Table
                        DT::dataTableOutput(outputId = "player_table"),
                        width = 12, # Width set to maximum (Bootstrap grid is out of 12)
                        height = 400, # Set height
                    )
                ),

                # 1.3 Plots ----
                fluidRow( 
                    box(plotOutput("plot1"), width = 6),
                    box(plotOutput("plot2"), width = 6)
                ),
                
                # 1.2 Filters ----
                fluidRow( 
                    box(
                        "Box content here", br(), "More box content",
                        sliderInput("slider", "Slider input:", 1, 100, 50),
                        textInput("text", "Text input:"),
                        width = 4
                    ),
                    box(
                        "Box content here", br(), "More box content",
                        sliderInput("slider", "Slider input:", 1, 100, 50),
                        textInput("text", "Text input:"),
                        width = 4
                    ),
                    box(
                        "Box content here", br(), "More box content",
                        sliderInput("slider", "Slider input:", 1, 100, 50),
                        textInput("text", "Text input:"),
                        width = 4
                    ),
                )
        ),
        # 2. Team Stats Tab ----
        tabItem(tabName = "teams_tab",
                h2("Teams Stats content"),
                # 2.1 Stats Table ----
                fluidRow(
                    box(
                        # See Data Table
                        DT::dataTableOutput(outputId = "team_table"),
                        width = 12, # Width set to maximum (Bootstrap grid is out of 12)
                        height = 400, # Set height
                    )
                ),
                
                # 2.2 Plots ----
                fluidRow( 
                    box(plotOutput("plot3"), width = 6),
                    box(plotOutput("plot4"), width = 6)
                ),
                
                # 2.3 Filters ----
                fluidRow( 
                    box(
                        "Box content here", br(), "More box content",
                        sliderInput("slider", "Slider input:", 1, 100, 50),
                        textInput("text", "Text input:"),
                        width = 4
                    ),
                    box(
                        "Box content here", br(), "More box content",
                        sliderInput("slider", "Slider input:", 1, 100, 50),
                        textInput("text", "Text input:"),
                        width = 4
                    ),
                    box(
                        "Box content here", br(), "More box content",
                        sliderInput("slider", "Slider input:", 1, 100, 50),
                        textInput("text", "Text input:"),
                        width = 4
                    ),
                )
                
        ),
        # 3. Standings Tab ----
        tabItem(tabName = "standings_tab",
                h2("Standings")
        )
    )
)


# Dashboard Page ----------------------------------------------------------

dash_page <- dashboardPage(header, sidebar, body, skin = "black")


# UI ----------------------------------------------------------------------


ui <- {

    # Dashboard
    dash_page
}


# # Define UI for the app
# ui <- fluidPage(
#     # Theme
#     theme = shinytheme("slate"),
#     # themeSelector(),
#     
#     # Custom CSS
#     tags$head(
#         tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
#     ),
# 
#     # NavBar
#     navbarPage(title = "PBA Analytics",
# 
#         # TabPanels
#         tabPanel("Player Stats", value = "player_stats",
#                 ## Using create_page function
#                 # create_page("Player Stats", data_table = "table")
#                 
#                 ## Using In-Line Code
#                 # Create a Shiny dashboard page
#                 dashboardPage(
#                     
#                     # Navbar: Set the title from the function argument
#                     dashboardHeader(title = "Player Stats"),
#                     
#                     # Sidebar: Add input controls
#                     dashboardSidebar(
#                         sidebarMenu(id = "sidebar_menu", # Sidebar ID for potential UI control
#                                     # Add dropdowns for year, season
#                                     selectInput(inputId = "year", label = "Select Year", choices = c("2021", "2020")),
#                                     selectInput(inputId = "season", label = "Select Season", choices = c("Spring", "Summer")),
#                                     selectInput(inputId = "team", label = "Select Team", choices = c("PBA", "OC", "VIU"))
#                                     
#                         )
#                     ),
#                     
#                     # Main Panel: Add content boxes
#                     dashboardBody(
#                         # Row 1: Table Box
#                         fluidRow(
#                             box(
#                                 # See Data Table
#                                 DT::dataTableOutput(outputId = "table"),
#                                 title = "Table",
#                                 width = 12, # Width set to maximum (Bootstrap grid is out of 12)
#                                 height = 400, # Set height
#                             )
#                         ),
#                         # Row 2: Plot Boxes
#                         fluidRow(
#                             box(
#                                 title = "Plot 1", # Box title
#                                 plotOutput("plot1") # Output ID for the first plot
#                             ),
#                             box(
#                                 title = "Plot 2", # Box title
#                                 plotOutput("plot2") # Output ID for the second plot
#                             )
#                         )
#                     )
#                 )
#         ),
#         tabPanel("Team Stats", value = "team_stats",
#                  create_page("Team Stats", data_table = "table")
#         ),
#         tabPanel("Standings", value = "standings",
#                  create_page("Standings", data_table = "table", standings = TRUE)
#         ),
#     )
# )


# Server ------------------------------------------------------------------


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
    
    # output$table <- renderDataTable(player_batting_df) # input$category ?
    
    # Create the output tables (needs changes)
    output$player_table <- DT::renderDataTable({ player_batting_df })
    output$team_table <- DT::renderDataTable({ team_batting_df })
}


# Run App -----------------------------------------------------------------


shinyApp(ui, server)
