
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


# Load in RDS Files -------------------------------------------------------

advanced_hitting_tbl  <-  readRDS("data/advanced_hitting_tbl.rds")
standard_hitting_tbl  <-  readRDS("data/standard_hitting_tbl.rds")
base_running_tbl      <-  readRDS("data/base_running_tbl.rds")
standard_pitching_tbl <-  readRDS("data/standard_pitching_tbl.rds")
team_stats_tbl        <-  readRDS("data/team_stats_final_tbl.rds")

# Dashboard Header --------------------------------------------------------


header <- dashboardHeader(title = "PBA Scouting Portal")



# Dashboard Sidebar -------------------------------------------------------


sidebar <- dashboardSidebar(
    sidebarMenu(
        # if id is present, this id will be used for a Shiny input value, 
        # and it will report which tab is selected. For example, if id="tabs", 
        # then input$tabs will be the tabName of the currently-selected tab
        id = "tabs",
        menuItem("Player Stats", tabName = "players_tab", icon = icon("baseball")),
        selectInput("dataset", "Dataset", c("Standard Hitting", "Advanced Hitting", 
                                            "Base Running", "Standard Pitching", "Team Stats")),
        selectInput(inputId = "year", label = "Select Year", choices = c("2021", "2020")),
        selectInput(inputId = "season", label = "Select Season", choices = c("Spring", "Summer")),
        selectInput(inputId = "team", label = "Select Team", choices = c("PBA", "OC", "VIU"))

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
                        DT::DTOutput(outputId = "player_table"),
                        width = 12, # Width set to maximum (Bootstrap grid is out of 12)
                        height = 400, # Set height
                    )
                ),

                # 1.2 Plots ----
                fluidRow( 
                    box(plotOutput("plot1"), width = 6),
                    box(plotOutput("plot2"), width = 6)
                ),
                
                # 1.4 Tab Test ----
                fluidRow(
                    textOutput("currentTab1")
                )
                
        )
    )
)


# UI ----------------------------------------------------------------------


ui <- {

    dashboardPage(header, sidebar, body, skin = "black")
}


# Server ------------------------------------------------------------------


server <- function(input, output, session) {
    
    # Select the Dataset
    datasetInput <- reactive({
        switch(input$dataset,
               "Standard Hitting" = standard_hitting_tbl,
               "Advanced Hitting" = advanced_hitting_tbl,
               "Base Running"     = base_running_tbl,
               "Standard Pitching" = standard_pitching_tbl,
               "Team Stats" = team_stats_tbl
               )
    })

    observe({
        
        # Print the current tab and stat category to the console
        print(paste("Current tab: ", input$tabs))
        # print(paste("Selected Category: ", input$category))
        
        # Get the current tab and require to be run
        # current_tab <- reactive({input$my_tabs})
        current_tab <- input$my_tabs
        req(current_tab)

    })
    
    
    # Render Text for Tab Testin
    output$currentTab1 <- renderText({
        paste("You are on tab:", input$tabs)
    })
    
    # output$player_table <- DT::datatable(datasetInput())
    
    # 
    # # Create the output tables (needs changes)
    # output$player_table <- DT::renderDataTable({ test() })
    output$player_table <- DT::renderDT({ 
        datasetInput() }, filter = c("top"))
}


# Run App -----------------------------------------------------------------


shinyApp(ui, server)
