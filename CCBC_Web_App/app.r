# Load required libraries
library(shiny)
library(shinydashboard)
library(shinythemes)

# Define UI for the app
ui <- fluidPage(
    # Title
    theme = shinytheme("slate"),
    # themeSelector(),
    # Custom CSS
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    navbarPage(title = "PBA Analytics",
               tabPanel("Player Stats",
            dashboardPage(
            
            # Navbar
            dashboardHeader(title = "Player Stats"),
            
            
            # Sidebar
            dashboardSidebar(
                tags$head(
                    tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css")
                ),
                sidebarMenu(id = "sidebar_menu",
                            selectInput("year", "Select Year", choices = c("2021", "2020")),
                            selectInput("season", "Select Season", choices = c("Spring", "Summer")),
                            selectInput("team", "Select Team", choices = c("PBA", "OC", "VIU")),
                            selectInput("category", "Stat Category", choices = c("Hitting", "Pitching"))
                )
            ),
            
            # Main Panel
            dashboardBody(
                tags$head(
                    tags$link(rel = "stylesheet", type = "text/css", href = "www/style.css")
                ),
                fluidRow(
                    box(
                        title = "Table",
                        width = 12,
                        height = 400,
                        dataTableOutput("stats_table")
                    ),
                ),
                fluidRow(
                    box(
                        title = "Plot 1",
                        plotOutput("plot1")
                    ),
                    box(
                        title = "Plot 2",
                        plotOutput("plot2")
                        )
                    ),
                )
            )
        )
    )
)

# Define server logic for the app
server <- function(input, output, session) {
    # Placeholder for server logic
}

# Run the app
shinyApp(ui, server)
