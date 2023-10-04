
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


# Change Double Play ------------------------------------------------------

standard_hitting_tbl <- standard_hitting_tbl %>% 
    rename(
        "GDP" = DP
    )

base_running_tbl <- base_running_tbl %>% 
    rename(
        "GDP" = DP
    )


# Constants ---------------------------------------------------------------

years <- unique(advanced_hitting_tbl$Year)
seasons <- levels(unique(advanced_hitting_tbl$Season))

# Dashboard Header --------------------------------------------------------


header <- dashboardHeader(title = "Up North Baseball")



# Dashboard Sidebar -------------------------------------------------------


sidebar <- dashboardSidebar(
    sidebarMenu(
        # if id is present, this id will be used for a Shiny input value, 
        # and it will report which tab is selected. For example, if id="tabs", 
        # then input$tabs will be the tabName of the currently-selected tab
        id = "tabs",
        menuItem("Welcome", tabName = "Welcome", icon = icon("door-open")),
        menuItem("Statistics Dictionary", tabName = "stat_dict", icon = icon("info")),
        menuItem("Dashboard", tabName = "dashboard", icon = icon("baseball")),
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
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
        # 1. General Information Tab ----
        tabItem(tabName = "Welcome",
                fluidRow(
                    box(
                        h3("Introduction"),
                        p("Hello! Welcome to Up North Baseball (name isn't set).
                          This site is the first home for advanced analytics for the CCBC.
                          While this is the first working version and many updates will hopefully
                          be made over time, I hope you enjoy your time here. If this is your first time
                          please read a bit to understand what you are looking at"),
                        width = 12
                    ),
                    box(
                        h3("About the Dashboard"),
                        p("The first version is shipped with 5 different datasets that can be selected on the sidebar. 
                          You can also use the filter to select the year(s), and season type(s). Note that it can get very messy
                          for preseason stats due to the large number of non-conference opponents. The last item on the sidebar 
                          is the input to the plot. Use this to create a plot of any statistic, grouped by the teams. You can also
                          search by Team and Player name. To do this,", em("Please use the Search bar in the top right corner"), ". 
                          To filter by the different statistics, click in the little box above the stat and select the range you want to see. 
                          You are also able to export this data to CSV or Excel with the button in the top right. Note that the export will only show 
                          the stats shown on the table you see (not the other tabs)."),
                        width = 12
                    ),
                    box(
                        h3("Limitations"),
                        tags$ol(
                            list(
                                tags$li("Only contains stats from qualified hitters and pitchers. 
                                        This means that if you are searching for a player that you know
                                        played in a certain year and can't find them, they likely did not
                                        qualify"),
                                tags$li("Not updated in real-time. Hopefully by the 2024 season the app will
                                        include real-time updates"),
                                tags$li("Sabermetrics are not always the same as MLB equivalents. We highly encourage 
                                        you to read the Statistics Dictionary for more information about each metric
                                        and how it is to be used"),
                                tags$li("The make a plot can be glitchy. It is designed to show the average per team which
                                        works fine with stats like OPS, AVG and wOBA but does not really work with counting statistics
                                        (like homeruns). This should be addressed in subsequent updates")
                            )
                        ),
                        width = 12
                    ),
                    box(
                        h3("Get Involved"),
                        p("The end goal of this early project is to be the site that hosts all Canadian baseball 
                          statistics from High School / Travel ball to College across the country. It is my belief that
                          better visibility and more accurate information will grow the game in Canada. If you have any
                          programming ability specifically in front end development (HTML/CSS/JS), software engineering (Docker),
                          or Shiny Apps If and you are curious about getting involved please email me ", 
                          a(href = "mailto:chris_ewanik@outlook.com", "chris_ewanik@outlook.com")),
                        width = 12
                    ),
                    box(
                        h3("About the Developer"),
                        p("This page is currently ran by Christopher Ewanik. Christopher is
                          a finishing a Master's Degree in Data Science & Engineering from the
                          University of Maine. Christopher played collegiately at Prairie Baseball Academy
                          before transferring to Husson University (NCAA DIII). Christopher is currently seeking 
                          internships and full time employment upon his tentative graduation date of May 2024"),
                        width = 12
                    )
                )
        ),
        
        # 2. Guidelines Tab ----
        tabItem(tabName = "stat_dict",
                h2("Guidelines and Instructions"),
                fluidRow(
                    box(
                        h3("Guidelines"),
                        p("This is where the guidelines text will go."),
                        width = 12
                    )
                )
        ),
        # 3. Dashboard ----
        tabItem(tabName = "dashboard",
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

    dashboardPage(header, sidebar, body, skin = "red")
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
                  filter = "top",
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
