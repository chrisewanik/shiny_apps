
# 1. Functions ------------------------------------------------------------



#' Create a Shiny Dashboard Page
#'
#' This function generates a Shiny dashboard page with pre-defined sidebar and body elements.
#' 
#' @param title Character string, the title to display in the dashboard header.
#'
#' @return A Shiny dashboardPage object.
#'
#' @examples
#' create_page("My Dashboard")
create_page <- function(title) {
    
    # Create a Shiny dashboard page
    dashboardPage(
        
        # Navbar: Set the title from the function argument
        dashboardHeader(title = title),
        
        # Sidebar: Add input controls
        dashboardSidebar(
            sidebarMenu(id = "sidebar_menu", # Sidebar ID for potential UI control
                        # Add dropdowns for year, season, team, and stat category
                        selectInput("year", "Select Year", choices = c("2021", "2020")),
                        selectInput("season", "Select Season", choices = c("Spring", "Summer")),
                        selectInput("team", "Select Team", choices = c("PBA", "OC", "VIU")),
                        selectInput("category", "Stat Category", choices = c("Hitting", "Pitching"))
            )
        ),
        
        # Main Panel: Add content boxes
        dashboardBody(
            # Row 1: Table Box
            fluidRow(
                box(
                    title = "Table",
                    width = 12, # Width set to maximum (Bootstrap grid is out of 12)
                    height = 400, # Set height
                    dataTableOutput("stats_table") # Output ID for the table
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
}

