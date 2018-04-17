library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(extrafont)

rm(list = ls())
loadfonts(device = "win")

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


standings %>% head()

standings %>%
  filter(Game.No>=82) %>% filter(Conference == "Western")
