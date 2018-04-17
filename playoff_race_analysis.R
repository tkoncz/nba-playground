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

win_totals_plot <- end_of_regular_season %>%
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
                        color = Conference), fatten = 1.35, size = 1.4) +
    geom_text(aes(x=-0.75, y = -45, label = "WEST"), size = 10, family = "OCR A Extended", hjust = 1) +
    geom_text(aes(x=-0.75, y =  45, label = "EAST"), size = 10, family = "OCR A Extended", hjust = 0) +
    geom_text(aes(x= 9, y = 70, label = "▼ Lottery"),  size = 5, family = "OCR A Extended", hjust = 0, vjust = 1, color = "gray82") +
    geom_text(aes(x= 8, y = 70, label = "▲ Playoffs"), size = 5, family = "OCR A Extended", hjust = 0, vjust = 0, color = "gray82") +
    coord_flip() +
    scale_x_reverse(breaks = 15:1, limits = c(15, -1)) +
    scale_y_continuous(breaks = c(-82, -60, -41, 0, 41, 60, 82), 
                       limit  = c(-82,  82), 
                       labels =  c(82,  60,  41, 0, 41, 60, 82)) +
    scale_color_manual(breaks = c("Eastern", "Western"), values=c("blue4", "red4")) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 8.5, linetype = "dashed", size = 1, color = "gray82") +
    labs(title = "Race across 3. - 10. seeds was tightest in recent years",
         subtitle = "Last four seasons - latest marked by dots",
         x = "End of season position",
         y = "Total Wins",
         caption = "Data source: basketball-reference.com") +
    theme_minimal() + 
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()) +
    theme(text          = element_text(size = 13, family = "OCR A Extended")) +
    theme(plot.title    = element_text(size = 19),
          plot.subtitle = element_text(color = "gray60"),
          plot.caption  = element_text(color = "gray60"),
          legend.position = "none") +
    theme(legend.position = "none")


ggsave(file = "win_totals_plot.png", plot = win_totals_plot, 
       dpi = 600, width = 30, height = 20, units = "cm", limitsize = FALSE)

?geom_vline
