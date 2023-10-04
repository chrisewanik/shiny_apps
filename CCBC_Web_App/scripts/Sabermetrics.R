# Header ---------------------------------------------------------------
# Script Name: Sabermetrics.R
# Author: Christopher Ewanik
# Created: 2023-10-03
# Purpose: A brief description of what the script does
# Acknowledgments: This script uses data from Pointstreak and Stats from FanGraphs
# Keywords: data wrangling, data visualization, Statistics


# Notes
# 1. I could not find a wOBA scale equation so I did wOBA/wOBA Coef and found
#       0.27 to be a close enough constant, this could be better
# 2. I do not have the data to calculate Park Factors so I set it to 1 (this 
#       would be really cool later and it is in your R Book)
# 3. Did not have IBBs so any stat with them has it omitted
# 4. Due to data limitations my calculation of c_Off is WRAA + wSB
# 5. I did a constant for runCS when I did the calc I didn't like the values (too low penalty for outs IMO)

# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(DBI)
# library(scales) Not run bc conflicts but here for reference


# Source Functions --------------------------------------------------------

source("scripts/sql_functions.R")


# Query DB ----------------------------------------------------------------

team_batting_df <- query_database(team_batting_query)
team_pitching_df <- query_database(team_pitching_query)
player_batting_df <- query_database(player_batting_query)
player_pitching_df <- query_database(player_pitching_query)
standings_df <- query_database(standings_query)


# Constants ---------------------------------------------------------------

wOBA_scale_const = 0.27
park_factor = 1
runSB = 0.2
runCS = -.407 # runCS = 2 x RunsPerOut + 0.075 (first shown is my rough const)
RPW = 10


# Create Player Hitting Stats ----------------------------------------------------

# Calculate OBP and SLG, then use them to get OPS
hitters_tbl <- player_batting_df %>%
    # Create Factor Variables
    mutate(season_type = as_factor(season_type)) %>% 
    
    mutate(
        # Calculate On-Base Percentage (OBP)
        OBP = (h + bb + hbp) / (ab + bb + hbp + sf),
        
        # Calculate Total Bases
        Total_Bases = h + 2*doubles + 3*triples + 4*hr,
        
        # Calculate Slugging Percentage (SLG)
        SLG = Total_Bases / ab,
        
        # Calculate Plate Appearances
        PA = ab + bb + hbp + sh + sf,
        
        # Calculate singles, which isn't directly in your dataset
        singles = h - (doubles + triples + hr),
        
        # Calculate Steal Percentage
        steal_percent = sb / (sb + cs),
        
        # Calculate wOBA using FanGraphs weights
        wOBA = (0.69 * bb + 0.72 * hbp + 0.89 * h + 1.27 * doubles + 1.62 * triples + 2.10 * hr) / 
            (ab + bb + sf + hbp),
        
        # Calculate BABIP 
        BABIP = (h - hr)/(ab - so - hr + sf),
        
        # Calculate ISO (Isolated Power)
        ISO = (SLG - avg),
        
        # Calculate K%
        k_perc = so / PA,
        
        # Calculate BB%
        bb_perc = bb / PA
    ) %>%
    mutate(
        # Calculate On-base Plus Slugging (OPS)
        OPS = OBP + SLG
    )


# Create League Average Stats ---------------------------------------------

# Calculate league averages
league_avg_tbl <- hitters_tbl %>%
    group_by(year) %>%
    summarise(
        league_avg_OBP = mean(OBP, na.rm = TRUE),
        league_avg_SLG = mean(SLG, na.rm = TRUE),
        league_avg_wOBA = mean(wOBA, na.rm = TRUE),
        league_avg_r_pa = mean(r/PA, na.rm=TRUE),
        league_pa = mean(PA, na.rm = TRUE),
        wScale = league_avg_wOBA/wOBA_scale_const
        # The values didn't quite look right
        # runCS = 2 * (mean(r, na.rm=TRUE) / (mean(g, na.rm=TRUE)*8*3)) + 0.075 # runCS = 2 x RunsPerOut + 0.075
    )



# Create Sabermetric Stats ------------------------------------------------




# Join league_avg_df with hitter_data
hitters_tbl <- left_join(hitters_tbl, league_avg_tbl, by = "year")


hitters_tbl <- hitters_tbl %>%
    mutate(
        # Calculate OPS+
        OPS_plus = (OPS / (league_avg_OBP + league_avg_SLG)) * 100,
        
        # Calculate wRAA
        wRAA = ((wOBA - league_avg_wOBA) / wScale ) * PA,
        
        # Calculate wRC
        wRC = (((wOBA - league_avg_wOBA)/wScale) + league_avg_r_pa) * PA) %>% 
    
    # Calculate league_avg_wRC (for wRC+)
    group_by(year) %>% 
    mutate(
        league_avg_wRC_pa = mean(wRC, na.rm = TRUE) / mean(PA, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
        
    mutate(
        # Calculate wRC+
        wRC_plus = (((wRAA/PA + league_avg_r_pa) + (league_avg_r_pa - park_factor * league_avg_r_pa)) / (league_avg_wRC_pa)) * 100
    ) %>% 
    
    # Calculate wSB (Weighted Stolen Base Runs)
    group_by(year) %>% 
    mutate(lgSB = mean(sb, na.rm = TRUE),
           lgCS = mean(cs, na.rm = TRUE),
           lg1B = mean(singles, na.rm = TRUE),
           lgBB = mean(bb, na.rm = TRUE),
           lgHBP = mean(hbp, na.rm = TRUE),
           lgwSB = (lgSB * runSB + lgCS * runCS) / (lg1B + lgBB + lgHBP)
           ) %>% 
    ungroup() %>% 
    mutate(wSB = sb * runSB + cs * runCS - lgwSB * (singles + bb + hbp)) %>% 
    
    
    mutate(
        # Calculate o_Off (Adjusted Off)
        a_Off = wRAA + wSB,
        
        # Calculate o_WAA
        o_WAA = a_Off / 10
        )


# Pitching Stats ----------------------------------------------------------

pitchers_tbl <- player_pitching_df %>% 
    
    # Create Factor Variables
    mutate(season_type = as_factor(season_type)) %>% 
    
    mutate(
        # Create WHIP
        WHIP = (bb + h) / ip,
        
        # Create K Per 9
        k_per_9 = so * 9 /ip,
        
        # Create BB per 9
        bb_per_9 = bb * 9 /ip,
        
        # Create unearned runs
        uer = r - er

    )


# Create RDS Files --------------------------------------------------------

# First the Standard Player Hitting Tbl
standard_hitting_tbl <- hitters_tbl %>%
    select(1,2,6, 3:5, 8,9, 28, 7, 25, 27, 36, 11, 29, 12:14, 26, 10, 15:20, 24:25, 23) %>% 
    mutate(
        OBP = round(OBP, 3),
        SLG = round(SLG, 3),
        OPS = round(OPS, 3)
    ) %>% 
    rename(
        "Last Name"     = last_name,
        "First Initial" = first_initial,
        "Team"          = team_abbr,
        "Year"          = year,
        "Season"        = season_type,
        "AVG"           = avg,
        "Games"         = g,
        "AB"            = ab,
        "Runs"          = r,
        "Hits"          = h,
        "1B"            = singles,
        "2B"            = doubles,
        "3B"            = triples,
        "HR"            = hr,
        "RBI"           = rbi,
        "BB"            = bb,
        "HBP"           = hbp,
        "K"             = so,
        "SF"            = sf,
        "SH"            = sh,
        "E"             = e,
        "P"             = p,
        "Total Bases"   = Total_Bases,
        "DP"            = dp
    ) 

# Save as an RDS (R Data Storage)
saveRDS(standard_hitting_tbl, "data/standard_hitting_tbl.rds")

# The Advanced View
advanced_hitting_tbl <- hitters_tbl %>%
    select(1, 2, 6, 3:5, 8, 9, 28, 34, 35, 25, 27, 36, 43, 31, 47, 56, 44, 54, 55, 32, 33) %>% 
    mutate(
        OBP      = round(OBP, 3),
        SLG      = round(SLG, 3),
        OPS      = round(OPS, 3),
        k_perc   = scales::percent(k_perc, 2),
        bb_perc  = scales::percent(bb_perc, 2),
        OPS_plus = round(OPS_plus, 0),
        wOBA     = round(wOBA, 3),
        wRC_plus = round(wRC_plus, 0),
        o_WAA    = round(o_WAA, 1),
        wRAA     = round(wRAA, 1),
        wSB      = round(wSB, 1),
        a_Off    = round(a_Off, 1),
        BABIP    = round(BABIP, 3),
        ISO      = round(ISO, 3)
    ) %>% 
    rename(
        "Last Name"     = last_name,
        "First Initial" = first_initial,
        "Team"          = team_abbr,
        "Year"          = year,
        "Season"        = season_type,
        "Games"         = g,
        "AB"            = ab,
        "K%"            = k_perc,
        "BB%"           = bb_perc,
        "OPS+"          = OPS_plus,
        "wRC+"          = wRC_plus,
        "Off"           = a_Off
    ) 

# Save as an RDS (R Data Storage)
saveRDS(advanced_hitting_tbl, "data/advanced_hitting_tbl.rds")

# Baserunning
base_running_tbl <- hitters_tbl %>%
    select(1, 2, 6, 3:5, 8, 9, 28, 19:22, 30, 54, 23) %>%
    mutate(
        steal_percent = scales::percent(steal_percent, 2),
        wSB           = round(wSB, 1)
    ) %>%
    rename(
        "Last Name"     = last_name,
        "First Initial" = first_initial,
        "Team"          = team_abbr,
        "Year"          = year,
        "Season"        = season_type,
        "Games"         = g,
        "AB"            = ab,
        "SF"            = sf,
        "SH"            = sh,
        "SB"            = sb,
        "CS"            = cs,
        "Steal%"        = steal_percent,
        "P"             = p,
        "DP"            = dp 
    )

# Save as an RDS (R Data Storage)
saveRDS(base_running_tbl, "data/base_running_tbl.rds")  


# Player Pitching ---------------------------------------------------------

standard_pitching_tbl <- pitchers_tbl %>% 
    select(1:7, 9, 20:23, 10, 11, 12, 24, 13:16, 8, 17:19) %>% 
    mutate(
        WHIP     = round(WHIP, 2),
        k_per_9  = round(k_per_9, 1),
        bb_per_9 = round(bb_per_9, 1)
    ) %>%
    rename(
        "Last Name"     = last_name,
        "First Initial" = first_initial,
        "Team"          = team_abbr,
        "Year"          = year,
        "Season"        = season_type,
        "Games"         = g,
        "Games Started" = gs,
        "IP"            = ip,
        "ERA"           = era,
        "K/9"           = k_per_9,
        "BB/9"          = bb_per_9,
        "Hits"          = h,
        "Runs"          = r,
        "Earned Runs"   = er,
        "Unearned Runs" = uer,
        "BB"            = bb,
        "K"             = so,
        "Wins"          = w,
        "Losses"        = l,
        "CG"            = cg,
        "SV"            = sv,
        "2B"            = doubles,
        "3B"            = triples
    )

# Save as an RDS (R Data Storage)
saveRDS(standard_pitching_tbl, "data/standard_pitching_tbl.rds") 



# Team Stats --------------------------------------------------------------


# Insert Missing PBA data and remove glitch --
team_pitching_df <- team_pitching_df %>% 
    # Remove year
    slice(-104)

# Create New Vector to be inserted
pba_row <- data.frame(team_abbr = "PBA", year = 2017, season_type = "CCBC", w = 20, l = 3,
                      ip = 191.2, r = 97, er = 85, h = 170, bb = 88, wp = 15, hbp = 10,
                      so = 237, bf = 846)

# To insert new_row at position 104
top <- team_pitching_df[1:103, ]  # Keep rows from 1 to 103
bottom <- team_pitching_df[104:nrow(team_pitching_df), ]  # Keep rows from 104 to the end

# Combine the top, new_row, and bottom
team_pitching_df <- bind_rows(top, pba_row, bottom)

# Insert Missing OC data--
oc_row <- data.frame(team_abbr = "OC", year = 2013, season_type = "CCBC", w = 11, l = 16,
                      ip = 207.0, r = 159, er = 129, h = 236, bb = 92, wp = 24, hbp = 16,
                      so = 152, bf = 973)

# To insert new_row at position 104
top <- team_pitching_df[1:197, ]  # Keep rows from 1 to 197
bottom <- team_pitching_df[198:nrow(team_pitching_df), ]  # Keep rows from 198 to the end

# Combine the top, new_row, and bottom
team_pitching_df <- bind_rows(top, oc_row, bottom)


# Create team_stats_tbl where batting is .x
team_stats_tbl <- left_join(team_batting_df, team_pitching_df, by = c("team_abbr", "year", "season_type"))

# Determine the Optimal Pythag Winning % Coef
pythag_tbl <- team_stats_tbl %>% 
    filter(season_type %in% c("CCBC")) %>% 
    select(w, l, r.x, r.y) %>% 
    mutate(logWratio = log(w/l),
           logRratio = log(r.x/r.y))

# fit the lm
pythag_fit <- lm(logWratio ~ 0 + logRratio, data = pythag_tbl)

# Store the coeff
pythag_coef <- pythag_fit$coefficients[[1]]

# Create final team_stats_tbl
team_stats_final_tbl <- team_stats_tbl %>% 
    mutate(RD = r.x - r.y,
           season_type = as_factor(season_type),
           WPCT = w / (w+l),
           xWPCT = (r.x**pythag_coef)/(r.x**pythag_coef + r.y**pythag_coef),
           xWins = round(xWPCT * g,0),
           WPCT_diff = WPCT - xWPCT,
           Win_diff = w - xWins) %>% 
    select(1:4, 14, 15, 26:30, 6, 17, 25, 5, 7:13, 16:24) %>% 
    mutate(
        WPCT = round(WPCT, 3),
        xWPCT = round(xWPCT, 3),
        WPCT_diff = round(WPCT_diff, 3)
    ) %>%
    rename(
        "Team"          = team_abbr,
        "Year"          = year,
        "Season"        = season_type,
        "Games"         = g,
        "Wins"          = w,
        "Losses"        = l,
        "Win%"          = WPCT,
        "xWin%"         = xWPCT,
        "Win% Diff"     = WPCT_diff,
        "Win Diff"      = Win_diff,
        "Runs"          = r.x,
        "Runs Against"  = r.y,
        "AB"            = ab,
        "Hits"          = h.x,
        "2B"            = doubles,
        "3B"            = triples,
        "HR"            = hr,
        "RBI"           = rbi,
        "Total Bases"   = tb,
        "BB"            = bb.x,
        "IP"            = ip,
        "Earned Runs"   = er,
        "Hits Against"  = h.y,
        "BB Against"    = bb.y,
        "WP"            = wp,
        "Hit Batters"   = hbp,
        "K"             = so,
        "Batters Faced" = bf
        
    )


# Save as an RDS (R Data Storage)
saveRDS(team_stats_final_tbl, "data/team_stats_final_tbl.rds") 

# EDA ---------------------------------------------------------------



# Get League average OBP
hitters_tbl %>% 
    group_by(year) %>% 
    summarize(avg_obp = mean(OBP, na.rm = TRUE)) %>% 
    ungroup() %>% 
    ggplot(aes(x = year, y = avg_obp)) +
    geom_col(fill = "red") +
    geom_text(aes(label = round(avg_obp, 3)), vjust = -0.5) +  # Round to 3 decimal places and position above the column
    coord_cartesian(ylim = c(0.3, NA)) +
    theme_light()

# Get League average OPS
hitters_tbl %>% 
    group_by(year) %>% 
    summarize(avg_ops = mean(OPS, na.rm = TRUE)) %>% 
    ungroup() %>% 
    ggplot(aes(x = year, y = avg_ops)) +
    geom_col(fill = "red") +
    geom_text(aes(label = round(avg_ops, 3)), vjust = -0.5) +  # Round to 3 decimal places and position above the column
    coord_cartesian(ylim = c(0.3, NA)) +
    theme_light()

# Get League average wOBA
hitters_tbl %>% 
    group_by(year) %>% 
    summarize(avg_wOBA = mean(wOBA, na.rm = TRUE)) %>% 
    ungroup() %>% 
    ggplot(aes(x = year, y = avg_wOBA)) +
    geom_col(fill = "red") +
    geom_text(aes(label = round(avg_wOBA, 3)), vjust = -0.5) +  # Round to 3 decimal places and position above the column
    coord_cartesian(ylim = c(0.3, NA)) +
    theme_light()

# League Average BABIP .336 overall
hitters_tbl %>% 
    group_by(year) %>% 
    summarize(avg_BABIP = mean(BABIP, na.rm=TRUE)) %>% 
    ungroup() %>% 
    ggplot(aes(x = year, y = avg_BABIP)) +
    geom_col(fill = "red") +
    geom_text(aes(label = round(avg_BABIP, 3)), vjust = -0.5) +  # Round to 3 decimal places and position above the column
    coord_cartesian(ylim = c(0.225, .4)) +
    theme_light()

# Investigating BB% --

# Extract the team_abbr values into a vector
team_abbr_vector <- hitters_tbl %>% 
    filter(season_type == "CCBC") %>% 
    pull(team_abbr) %>% 
    unique()

# See BB% by Team
hitters_tbl %>% 
    filter(team_abbr %in% team_abbr_vector) %>% 
    group_by(team_abbr) %>% 
    summarise(BB_perc = mean(bb_perc, na.rm = TRUE)) %>% 
    arrange(desc(BB_perc))


# Investigating OPS --
hitters_tbl %>% 
    filter(team_abbr %in% team_abbr_vector) %>% 
    group_by(team_abbr) %>% 
    summarise(OPS = mean(OPS, na.rm = TRUE)) %>% 
    arrange(desc(OPS))



