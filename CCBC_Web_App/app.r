
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
                          While this is the first working version, and many updates will hopefully
                          be made over time, I hope you enjoy your time here. If this is your first time,
                          please read a bit to understand what you are looking at"),
                        width = 12
                    ),
                    box(
                        h3("About the Dashboard"),
                        p("The first version is shipped with 5 different datasets that can be selected on the sidebar. 
                          You can also use the filter to select the year(s) and season type(s). Note that preseason stats can get very messy
                          due to the large number of non-conference opponents. The last item on the sidebar 
                          is the input to the plot. Use this to plot any statistic grouped by the teams. You can also
                          search by Team and Player name. To do this,", em("Please use the Search bar in the top right corner"), ". 
                          To filter by the different statistics, click the little box above the stat and select the range you want to see. 
                          You can also export this data to CSV or Excel with the button in the top right. Note that the export will only show 
                          the stats on the table you see (not the other tabs)."),
                        width = 12
                    ),
                    box(
                        h3("Limitations"),
                        tags$ol(
                            list(
                                tags$li("Only contains stats from qualified hitters and pitchers. 
                                        This means that if you are searching for a player that you know
                                        played in a particular year and can't find them, they likely did not
                                        qualify"),
                                tags$li("Not updated in real-time. Hopefully, by the 2024 season, the app will
                                        include real-time updates"),
                                tags$li("Sabermetrics are not always the same as MLB equivalents. We highly encourage 
                                        you to read the Statistics Dictionary for more information about each metric
                                        and how it is to be used"),
                                tags$li("The make a plot can be glitchy. It is designed to show the average per team, which
                                        works fine with stats like OPS, AVG and wOBA but does not really work with counting statistics
                                        (like home runs). This should be addressed in subsequent updates. In the meantime, stick to averages not counts.")
                            )
                        ),
                        width = 12
                    ),
                    box(
                        h3("Get Involved"),
                        p("The end goal of this early project is to be the site that hosts all Canadian baseball 
                          statistics from High School / Travel ball to College across the country. I believe
                          better visibility and more accurate information will grow the game in Canada. If you have any
                          programming ability specifically in front-end development (HTML/CSS/JS), databases (SQL or NoSQL), software engineering (Docker),
                          general data science (Python or R) or Shiny Apps and are curious about getting involved, please email me ", 
                          a(href = "mailto:chris_ewanik@outlook.com", "chris_ewanik@outlook.com")),
                        width = 12
                    ),
                    box(
                        h3("About the Developer"),
                        p("This page is currently run by Christopher Ewanik. Christopher is
                          finishing a Master's Degree in Data Science & Engineering from the
                          University of Maine. Christopher played collegiately at Prairie Baseball Academy
                          before transferring to Husson University (NCAA DIII). Christopher is currently seeking 
                          internships and full-time employment upon his tentative graduation date of May 2024"),
                        width = 12
                    )
                )
        ),
        
        # 2. Guidelines Tab ----
        tabItem(tabName = "stat_dict",
                h2("Statistics Dictionary"),
                fluidRow(
                    box(
                        h3("A Quick Note"),
                        p("Almost all of these stats are implemented with the help of Fangraphs. I highly encourage you to learn by 
                          visiting ", a(href="https://library.fangraphs.com/getting-started/", "FanGraphs"), 
                          "Please note that I did not calculate any ballpark adjustments or positional adjustments for players. I imagine that ballpark adjustments make huge differences, especially considering the weather in Lethbridge. Positional adjustments make substantial differences in positional adjustments. "),
                        width = 12
                    ),
                    # For Hitting and Base Running Stats
                    box(
                        h3("Hitting and Base Running Stats"),
                        tags$ol(
                            list(
                                tags$li("G (Games Played): Number of games in which the player has appeared."),
                                tags$li("P (Listed Position): The listed position on Pointstreak (often inaccurate)."),
                                tags$li("Season: The category of season when the assigned statistics are true."),
                                tags$li("PA (Plate Appearances): Number of times the player has come to the plate."),
                                tags$li("HR (Home Runs): Number of home runs."),
                                tags$li("R (Runs Scored): Number of runs scored."),
                                tags$li("RBI (Runs Batted In): Number of times a run scores due to a batter’s plate appearance, not counting situations in which an error caused the run to score or the batter hit into a double play."),
                                tags$li("SB (Stolen Bases): Number of stolen bases."),
                                tags$li("H (Hits): Number of hits."),
                                tags$li("1B (Singles): Number of singles."),
                                tags$li("2B (Doubles): Number of doubles."),
                                tags$li("3B (Triples): Number of triples."),
                                tags$li("BB (Walks): Total number of walks (includes IBB)."),
                                tags$li("K (Strikeouts): Number of strikeouts."),
                                tags$li("HBP (Hit By Pitches): Number of times the batter reached after being hit by a pitch."),
                                tags$li("SF (Sacrifice Flies): Number of times a batter’s fly out allowed a runner to tag up and score."),
                                tags$li("SH (Sacrifice Bunts): Any bunt in which there was a runner on base, less than two outs where the batter was put out, and at least one runner advanced."),
                                tags$li("GDP (Grounded into Double Play): Number of times the batter hit into a double play."),
                                tags$li("E (Errors): Defensive Errors"),
                                tags$li("SB (Stolen Bases): Number of stolen bases."),
                                tags$li("CS (Caught Stealing): Number of times caught stealing."),
                                tags$li("AVG (Batting Average): Rate of hits per at bat, calculated as H/AB."),
                                tags$li("OBP (On Base Percentage): Rate at which the batter reaches base, calculated as (H+BB+HBP)/(AB+BB+HBP+SF)."),
                                tags$li("SLG (Slugging Percentage): Average number of total bases per at bat, calculated as Total Bases/AB."),
                                tags$li("On-base Plus Slugging (OPS) is the sum of a player’s on-base percentage and slugging percentage. Many sabermetricians don’t like OPS because it treats OBP as equal in value to SLG, while OBP is roughly twice as important as SLG regarding its effect on run-scoring (x1.8 to be exact). However, OPS has value as a metric because it is accepted and used more widely than other, more accurate statistics while also being a relatively accurate representation of offence. "),
                                tags$li("On-base Plus Slugging Plus (OPS+) This statistic normalizes a player’s OPS and puts the statistic on an easy-to-understand scale. A 100 OPS+ is the league average, and each point up or down is one percentage point above or below league average.  In other words, if a player had a 90 OPS+ last season, their OPS was 10% below the league average. "),
                                tags$li("BB% (Walk Percentage): Frequency with which the batter has walked, calculated as walks divided by plate appearances."),
                                tags$li("K% (Strikeout Percentage): Frequency with which the batter has struck out, calculated as strikeouts divided by plate appearances."),
                                tags$li("ISO (Isolated Power): Average number of extra bases per at bat, calculated several ways such as SLG minus AVG."),
                                tags$li("BABIP (Batting Average on Balls in Play): The rate at which the batter gets a hit when he puts the ball in play, calculated as (H-HR)/(AB-K-HR+SF). BABIP is essential because the frequency with which a player gets a hit on a ball in play or allows a hit on a ball in play is very telling. Three main factors influence BABIP; all three tell us something important about that player’s overall stat line. Those factors are defence, luck, and talent level. For hitters, we use BABIP as a sanity test that tells us if their overall batting line is sustainable. Virtually no MLB hitter can regularly produce a BABIP of .380 (CCBC tops is around .425) or higher, and anything in the .230 range is also very atypical for a major league hitter. In other words, BABIP allows us to see if a hitter seems to be getting a boost from poor defence or good luck or getting docked for facing good defences and having bad luck. Most people familiar with BABIP have a pretty good idea about why it’s essential, but using it responsibly and appropriately is much more challenging. We know that the league average BABIP is almost always right around .300 (.225 in the CCBC), so many people look at a player’s BABIP, and if it is significantly different from .300, they assume that the player is either very lucky or very unlucky. This is not always the appropriate way to think about BABIP. For hitters, you typically want to adjust your expectations toward that player’s career average rather than the league average. Batters have much more control over their BABIP than pitchers, which is another way of saying that a higher percentage of batter BABIP is controlled by actual talent levels. It’s certainly possible for hitters to improve their offensive game and raise their BABIP, but short, dramatic spikes are usually due to luck."),
                                tags$li("wOBA (Weighted On Base Average): Combines all the different aspects of hitting into one metric, weighing each in proportion to their actual run value. While batting average, on-base percentage, and slugging percentage fall short in accuracy and scope, wOBA measures and captures offensive value more accurately and comprehensively. In most situations, OPS and wOBA will lead you to similar conclusions. However, if you care about determining how well a player contributes to run scoring, wOBA is a more accurate representation of that contribution. OPS undervalues getting on base relative to hitting for extra bases and does not correctly weigh each type of extra-base hit. wOBA is based on a simple concept: Not all hits are created equal. Batting average assumes that they are, and on-base percentage does too, but is more accurate by including other ways of reaching base such as walking or being hit by a pitch. Slugging percentage weighs hits, but not accurately (Is a double worth twice as much as a single? In short, no) and again ignores other ways of reaching base. On-base plus slugging (OPS) does attempt to combine the different aspects of hitting into one metric, but it assumes that one percentage point of SLG is the same as that of OBP. A handy estimate is that OBP is around twice as valuable as SLG (the exact ratio is x1.8)."),
                                tags$li("Weighted Runs Above Average (wRAA) measures the number of offensive runs a player contributes to their team compared to the average player. How much offensive value did Evan Longoria contribute to his team in 2009? With wRAA, we can answer that question: 28.3 runs above average. A wRAA of zero is league-average, so a positive wRAA value denotes above-average performance, and a negative wRAA denotes below-average performance. This is also a counting statistic (like RBIs), so players accrue more (or fewer) runs as they play."),
                                tags$li("Off (Offensive Runs Above Average): Number of runs above or below average a player has been worth offensively, combining Batting Runs and wSB. (Our metric lacks some other factors that FanGraphs has.)"),
                                tags$li("oWAA (Wins Above Average): My attempt at recreating the WAR stat from MLB. This attempts to estimate the number of wins a player has been worth to his team through his offence compared to the average qualifying player. Calculated as the sum of WRAA + wSB. We assume a Win is worth roughly 10 runs, similar to MLB. "),
                                tags$li("ISO (Isolated Power): Average number of extra bases per at bat, calculated several ways such as SLG minus AVG. Tells you how often a player hits for extra bases. ISO is useful because two players with identical batting averages can have very different seasons, and two players with the same slugging percentages can have very different seasons, even if you hold walks, plate appearances, park effects, and luck constant. A .300 average with very few extra base hits is quite different from a .300 average with 40 home runs. The same is true of a .500 slugging percentage driven by many singles versus one driven by many doubles and home runs. ISO tells you the average number of extra bases a player gets per at bat, and this is a piece of information you want to know. You want to know what share of a player’s hits and singles and what share are extra base hits. ISO doesn’t tell you anything you can’t learn from AVG and SLG together, but it saves you a step."),
                                tags$li("wSB (Weighted Stolen Base Runs Above Average): Number of runs above or below average added by a player on stolen base attempts."),
                                tags$li("wRC+ (Weighted Runs Created Plus): Number of runs a player has generated for his team as a result of his wOBA and playing time normalized to an average value of 100. wRC is a measure of raw production and should be used as such, but remember it is not park, league, or position adjusted. Using wRC+ is even easier because the league average for position players is always 100. If a player has a 110 wRC+, you know they are ten percentage points better than the league average offensively. This is an excellent tool for comparing the at-bat-by-at-bat offensive performance of any two players in the league. Note that this is not ballpark-adjusted like the MLB version. This has no impact on comparing players on the same team but makes team-by-team comparisons less accurate (i.e. hitting in Lethbridge vs. Vancouver Island)"),
                                tags$li("wRAA (Weighted Runs Above Average): Number of runs above or below average a player has added as a hitter.")
                            )
                        ),
                        width = 12
                    ),
                    
                    # For PITCHING
                    box(
                        h3("PITCHING"),
                        tags$ol(
                            list(
                                tags$li("W (Wins): Number of wins."),
                                tags$li("L (Losses): Number of losses."),
                                tags$li("ERA (Earned Run Average): The average number of earned runs a pitcher allows per 9 innings. ((ER*9)/IP)."),
                                tags$li("G (Games): Number of games in which the pitcher appeared."),
                                tags$li("GS (Games Started): Number of games the pitcher started."),
                                tags$li("CG (Complete Games): Number of starts in which the pitcher recorded every out of an official game."),
                                tags$li("SV (Saves): Number of saves."),
                                tags$li("IP (Innings Pitched): Number of total innings pitched (.1 represents 1/3 of an inning, .2 represents 2/3 of an inning)."),
                                tags$li("H (Hits Allowed): Number of hits allowed by the pitcher."),
                                tags$li("R (Runs Allowed): Number of runs allowed by the pitcher."),
                                tags$li("ER (Earned Runs Allowed): Number of earned runs allowed by the pitcher, determined by the official scorer."),
                                tags$li("BB (Walks): Number of walks allowed by the pitcher."),
                                tags$li("HBP (Hit By Pitches): Number of hit batters."),
                                tags$li("WP (Wild Pitches): Number of wild pitches."),
                                tags$li("SO (Strikeouts): Number of strikeouts."),
                                tags$li("K/9 (Strikeouts per 9 innings): Average number of strikeouts per 9 innings."),
                                tags$li("BB/9 (Walks per 9 innings): Average number of walks per 9 innings."),
                                tags$li("WHIP (Walks Plus Hits per Inning Pitched): The average number of base runners allowed via hit or walk per inning.")
                            )
                        ),
                        width = 12
                    ),
                    box(
                        h3("Team Stats"),
                        tags$ol(
                            list(
                                tags$li("xWin% (Pythagorean Winning Percentage) – A metric to predict wins given expected runs scored and against. It is interesting (and actionable) to identify teams or seasons that had significant deviations from their expectations. This could be some other intangible influencing their performance not captured in the runs scored/allowed by the team (i.e. clutch, grit, bad team culture, etc.)."),
                                tags$li("xWins – The number of wins that xWin% expects from a team in a season. For example, if a team had an xWin% of .600 and played 30 games, their xWins would be 18. "),
                                tags$li("Win% Diff – The difference between their expected and actual winning percentages (Win% - xWin%). Negative values indicate a team underperformed their expected performance. Positive values indicate the team overperformed their expectation. An excellent example of a team with a negative Win% Diff wins lots of 1 or 2-run games but loses lots by 5 or 6 and ends the season with a .500 winning percentage and a negative run differential."),
                                tags$li("Win Diff  - The difference between wins and expected wins (i.e. Wins – xWins)"),
                                tags$li("RD – Run Differential (i.e. Runs Scored – Runs Against)")
                            )
                        ),
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
               Year %in% input$year,
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
