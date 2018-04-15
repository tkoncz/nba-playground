library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)

rm(list = ls())

schedule <- fread(file = "C:/Users/tkonc/Documents/Data/NBA Playground/schedulesTable.csv") 

## Conference / Division
teams <- fread(file = "C:/Users/tkonc/Documents/Data/NBA Playground/Conference_Division_Team.csv") 

source("standings_functions.R")

season <- schedule %>%
            filter(Regular.Or.Playoff == "Regular Season")

s = "2017-2018"

games <- fun_create_game_list(df_of_schedule = season, 
                              season_id      = s, 
                              df_of_teams    = teams)


winpct_list <- fun_create_winpct_matrices(games)

winpct_by_opponent   <- winpct_list[[1]]
winpct_by_division   <- winpct_list[[2]]
winpct_by_conference <- winpct_list[[3]]


# standings <- fun_base_standings(games)

standings <- fun_create_standings(df_of_games                 = games,
                                  df_of_winpct_by_opponent    = winpct_by_opponent,
                                  df_of_winpct_by_division    = winpct_by_division,
                                  df_of_winpct_by_conference  = winpct_by_conference)


## VISUALS

# games %>%
#   filter(Team %in% west_teams) %>%
#   ggplot(aes(x= Game.No, y= Win.Total, color = Team)) +
#   geom_line() +
#   theme_minimal()

# 
standings %>%
  filter(Game.No >= 41) %>%
  filter(Team %in% c("Los Angeles Clippers",
         "New Orleans Pelicans",
         "San Antonio Spurs",
         "Denver Nuggets",
         "Minnesota Timberwolves",
         "Oklahoma City Thunder",
         "Portland Trail Blazers",
         "Utah Jazz")) %>%
  ggplot(aes(x= Game.No, y= Conference.Rank, color = Team)) +
    geom_line(size = 1.05) +
    geom_point(size = 2) +
    scale_y_reverse() +
    facet_grid(~Conference) +
    theme_minimal()

standings %>%
  filter(Game.No>=82) %>% filter(Conference == "Western")
