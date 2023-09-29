# Create RDS DataFrames ---------------------------------------------------

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
        bb_per_9 = bb * 9 /ip

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
        k_perc   = scales::percent(round(k_perc, 1)),
        bb_perc  = scales::percent(round(bb_perc, 1)),
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



