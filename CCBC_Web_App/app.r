
# Load Libraries ----------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(DBI)
library(DT)
library(plotly)


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
        selectInput(inputId = "team", label = "Select Team", choices = c("PBA", "OC", "VIU")),
        selectInput(inputId = "input_var", label = "Plot Var", 
                    choices = mtcars %>% colnames(),
                    selected = "mpg")

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
                        height = 950, # Set height
                    )
                ),

                # 1.2 Plots ----
                fluidRow( 
                    box(# Output plot
                        plotlyOutput("barplot"), width = 12),
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

    # Observe Statement for Testing UI
    observe({
        # Print the current tab and stat category to the console
        print(paste("Current tab: ", input$tabs))
        
        # Get the current tab and require to be run
        current_tab <- input$my_tabs
        req(current_tab)
    })
    
    
    # Render Text for Tab Testin
    output$currentTab1 <- renderText({
        paste("You are on tab:", input$tabs)
    })
    
    # Render the DataTable
    output$player_table <- DT::renderDT({ 
        datatable(datasetInput(),
                  rownames = FALSE,
                  # filter = "top",
                  # CSS Class
                  class = "row-border compact stripe",
                  options = list(
                    autoWidth=TRUE,
                    scrollX = TRUE,
                    pageLength = 25
                    )
                  )
    })
    
    # Render our Barplot
    output$barplot <- renderPlotly({
        
        # Sort the selected variable in descending order and take top 10
        sorting_col <- input$input_var
        top_10_values <- mtcars %>% 
            # get allows you to refer to the column name dynamically
            arrange(desc(get(sorting_col))) %>% 
            head(10)
        
        # Generate barplot
        plot <- plot_ly(
            top_10_values, 
            x = ~rownames(top_10_values), 
            y = as.formula(paste("~`", sorting_col, "`", sep="")),
            type = 'bar',
            marker = list(color = 'rgb(158,202,225)')
        ) %>% 
            layout(
                title = paste("Top 10 values for", sorting_col),
                xaxis = list(title = ""),
                yaxis = list(title = sorting_col),
                template = "plotly_white" # Light theme
            )
        
        return(plot)
    })
    
}


# Run App -----------------------------------------------------------------


shinyApp(ui, server)
