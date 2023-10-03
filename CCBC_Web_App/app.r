
# Load Libraries ----------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(DBI)
library(DT)
library(plotly)
library(tidyquant)
library(scales)


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


# Constants ---------------------------------------------------------------

years <- unique(advanced_hitting_tbl$Year)
seasons <- levels(unique(advanced_hitting_tbl$Season))

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
        checkboxGroupInput(inputId = "year", label = "Select Year", 
                    choices = years, selected = 2023),
        checkboxGroupInput(inputId = "season", label = "Select Season", 
                    choices = seasons, selected = "CCBC"),
        selectInput(inputId = "input_var", label = "Plotting Variable", "Team")

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
                        DT::DTOutput(outputId = "table"),
                        width = 12, # Width set to maximum (Bootstrap grid is out of 12)
                        height = 800, # Set height
                    )
                ),

                # 1.2 Plots ----
                fluidRow( 
                    # Output plot
                    box(plotlyOutput("team_plot"), 
                        width = 12)
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
    
    # Filter the dataset
    dataset_filtered <- reactive({
        req(input$year)
        req(input$season)
        filter(datasetInput(), 
               Year == input$year,
               Season %in% input$season)
    })
    
    # Render the DataTable
    output$table <- DT::renderDT({ 
        datatable(dataset_filtered(),
                  extensions = 'Buttons',
                  rownames = FALSE,
                  # filter = "top",
                  # CSS Class
                  class = "row-border compact stripe",
                  options = list(
                    autoWidth=TRUE,
                    scrollX = TRUE,
                    pageLength = 20,
                    # keep search and paging
                    dom = 'Bfrtip',
                    buttons = c('csv', 'excel')
                    )
                  )
    })
    
    #
    observe({
        updateSelectInput(session, "input_var", 
                          choices = unique(dataset_filtered() %>% colnames()),
                          selected = tail(unique(colnames(dataset_filtered())), 1))
    })
    
    
    # Render our Team Plot
    
    output$team_plot <- renderPlotly({
        
        # Make sure input$input_var exists and is not NULL
        req(input$input_var)
        
        # Calculate the summarized data and min/max for the selected variable
        hitting_data <- dataset_filtered() %>%
            group_by(Team) %>%
            summarise_if(is.numeric, mean, na.rm = TRUE) %>%
            ungroup() %>%
            mutate(Team = fct_reorder(Team, .data[[input$input_var]]))  # Use the selected variable for ordering
            # .data[[input$input_var]] allows us to refer to columns in the dataframe using a variable (in this case, input$input_var)
        
        # Calculate min and max of the selected variable
        min_val <- min(hitting_data[[input$input_var]], na.rm = TRUE) / 1.5
        max_val <- max(hitting_data[[input$input_var]], na.rm = TRUE) * 1.15
        
        # Generate the ggplot
        g <- ggplot(hitting_data, aes(x = Team, y = .data[[input$input_var]])) +  # Use the selected variable for plotting
            geom_bar(stat="identity", color='red', fill='red') +
            coord_cartesian(ylim = c(min_val, max_val))
        
        ggplotly(g)
        
    })
    
}


# Run App -----------------------------------------------------------------


shinyApp(ui, server)
