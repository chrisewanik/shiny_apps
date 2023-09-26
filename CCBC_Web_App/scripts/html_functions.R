
# 1. Functions ------------------------------------------------------------



##' Create a Shiny Dashboard Page
#'
#' This function generates a Shiny dashboard page with pre-defined sidebar and body elements.
#' 
#' @param title Character string, the title to display in the dashboard header.
#' @param standings Logical, whether to remove team and category filters. Default is FALSE.
#'
#' @return A Shiny dashboardPage object.
#'
#' @examples
#' create_page("My Dashboard")
#' create_page("My Dashboard", TRUE)
create_page <- function(title, data_table, standings = FALSE) {
    
    # Create a Shiny dashboard page
    dashboardPage(
        
        # Navbar: Set the title from the function argument
        dashboardHeader(title = title),
        
        # Sidebar: Add input controls
        dashboardSidebar(
            sidebarMenu(id = "sidebar_menu", # Sidebar ID for potential UI control
                        # Add dropdowns for year, season
                        selectInput(inputId = "year", label = "Select Year", choices = c("2021", "2020")),
                        selectInput(inputId = "season", label = "Select Season", choices = c("Spring", "Summer")),
                        selectInput(inputId = "team", label = "Select Team", choices = c("PBA", "OC", "VIU")),
                        
                        # Add or remove filters based on 'standings' parameter
                        if (standings) {
                            # No additional filters for 'standings' page
                        } else {
                            # Weird Comma Error
                            selectInput(inputId = "category", label = "Stat Category", choices = c("Hitting", "Pitching"))
                        }
            )
        ),
        
        # Main Panel: Add content boxes
        dashboardBody(
            # Row 1: Table Box
            fluidRow(
                DT::dataTableOutput(outputId = data_table),
                box(
                    # See Data Table
                    # DT::dataTableOutput(outputId = data_table),
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
}


