library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(extrafont)

rm(list = ls())
#loadfonts(device = "win")

schedule <- fread(file = "C:/Users/tkonc/Documents/Data/NBA Playground/schedulesTable.csv") 

## Conference / Division
teams <- fread(file = "C:/Users/tkonc/Documents/Data/NBA Playground/Conference_Division_Team.csv") 

source("standings_functions.R")

season <- schedule %>%
  filter(Regular.Or.Playoff == "Regular Season")

seasons <- season %>% 
             pull(Season) %>% 
             unique()
#seasons = c("2017-2018")

no_of_seasons <- length(seasons)
season_standing_list <- vector("list", no_of_seasons)

i = 1
for(s in seasons) {
  games <- fun_create_game_list(df_of_schedule = season, 
                                season_id      = s, 
                                df_of_teams    = teams)
  
  
  winpct_list <- fun_create_winpct_matrices(games)
  
  winpct_by_opponent   <- winpct_list[[1]]
  winpct_by_division   <- winpct_list[[2]]
  winpct_by_conference <- winpct_list[[3]]
  
  standings <- fun_create_standings(df_of_games                 = games,
                                    df_of_winpct_by_opponent    = winpct_by_opponent,
                                    df_of_winpct_by_division    = winpct_by_division,
                                    df_of_winpct_by_conference  = winpct_by_conference)
  
  standings <- standings %>%
                 mutate(season = s) %>%
                 select(everything(), -Tie.Count)
  
  season_standing_list[[i]] <- standings
  i = i + 1
  
  print(s)
}

standings <- do.call(rbind, season_standing_list)

standings %>% 
  group_by(season) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = season, y= n)) + 
    geom_bar(stat = "identity") + 
    coord_flip()

standings_backup <- standings

standings <- standings %>%
               filter(!season %in% c("2004-2005", "2011-2012"))


end_of_regular_season <- standings %>%
                           filter(Game.No == 82)

# end_of_regular_season %>%
#   filter(season %in% c("2014-2015", "2015-2016", "2016-2017", "2017-2018")) %>%
#   ggplot(aes(x = Conference.Rank, y = Win.Total, fill = season)) +
#     geom_density(stat = "identity", alpha = 0.25) +
#   geom_vline(xintercept = 8) +
#     facet_grid(~Conference) +
#     theme_minimal()

end_of_regular_season %>%
  filter(season %in% c("2014-2015", "2015-2016", "2016-2017", "2017-2018")) %>%
  group_by(Conference, Conference.Rank) %>%
  mutate(Min.Win.Total = min(Win.Total),
         Max.Win.Total = max(Win.Total)) %>%
  ungroup() %>%
  filter(season == "2017-2018") %>%
  select(Conference, Conference.Rank, Win.Total, Min.Win.Total, Max.Win.Total) %>%
  mutate(Current.Win.Total.For.Chart = ifelse(Conference == "Western", -1 * Win.Total, Win.Total),
         Min.Win.Total.For.Chart     = ifelse(Conference == "Western", -1 * Min.Win.Total, Min.Win.Total),
         Max.Win.Total.For.Chart     = ifelse(Conference == "Western", -1 * Max.Win.Total, Max.Win.Total)) %>%
  select(Conference, Conference.Rank, Current.Win.Total.For.Chart, Min.Win.Total.For.Chart, Max.Win.Total.For.Chart) %>%
  mutate(Conference                  = as.factor(Conference),
         Conference.Rank             = as.integer(Conference.Rank),
         Current.Win.Total.For.Chart = as.integer(Current.Win.Total.For.Chart),
         Min.Win.Total.For.Chart     = as.integer(Min.Win.Total.For.Chart),
         Max.Win.Total.For.Chart     = as.integer(Max.Win.Total.For.Chart)) %>%
  ggplot(aes(x = Conference.Rank)) +
    geom_pointrange(aes(ymin = Min.Win.Total.For.Chart, 
                        ymax = Max.Win.Total.For.Chart, 
                        y = Current.Win.Total.For.Chart, 
                        color = Conference)) +
    coord_flip() +
    scale_x_reverse(breaks = 15:1) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 8.5) +
    theme_minimal() + 
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()) +
    theme(legend.position = "none")

?geom_pointrange
