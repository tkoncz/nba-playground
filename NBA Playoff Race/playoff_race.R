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


## VISUALS
colors <- fread(file = "C:/Users/tkonc/Documents/Data/NBA Playground/TeamColors.csv")
colors <- colors %>%
            mutate(rgb = rgb(R, G, B, maxColorValue = 255)) %>%
            select(Team, HEX)

games %>%
  left_join(colors, by = "Team") %>%
  filter(Conference == "Eastern") %>%
  ggplot(aes(x= Game.No, y= Win.Total, color = Team)) +
  geom_line() +
  scale_colour_manual(breaks = games$Team, values = as.character(colors$HEX)) +
  theme_minimal()

# 
west_playoff_race_teams <- c("Los Angeles Clippers",
                             "New Orleans Pelicans",
                             "San Antonio Spurs",
                             "Denver Nuggets",
                             "Minnesota Timberwolves",
                             "Oklahoma City Thunder",
                             "Portland Trail Blazers",
                             "Utah Jazz")

chart_data <- standings %>%
                filter(Game.No >= 60) %>%
                filter(Team %in% west_playoff_race_teams) %>%
                select(Game.No, Team, Conference.Rank)

chart_colors <- colors %>%
                  filter(Team %in% west_playoff_race_teams)

chart_legend <- standings %>%
                    filter(Game.No == 82) %>%
                    filter(Team %in% west_playoff_race_teams) %>%
                    select(Game.No, Team, Conference.Rank)

west_playoff_race_plot <- ggplot(data = chart_data, aes(x= Game.No, y= Conference.Rank, color = Team)) +
  geom_line(size = 1.4) +
  geom_point(size = 6, shape = 18) +
  geom_text(data = chart_legend, aes(label = Team, x = 83, y = Conference.Rank), hjust = 0, vjust = 0.5, fontface = "bold") +
  scale_y_reverse(breaks = c(10, 9, 8, 7, 6, 5, 4, 3)) +
  scale_colour_manual(breaks = chart_colors$Team, values = as.character(chart_colors$HEX)) +
  scale_x_continuous(breaks = seq(60, 82, 2), limit = c(60, 90)) +
  labs(title = "Teams in the West had some wild swings in the Standings",
       subtitle = "Playoff Race among teams between 3. and 10. seed",
       x = "Game Number",
       y = "Playoff position",
       caption = "Data source: basketball-reference.com, end of game results") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  theme(text          = element_text(size = 13, family = "OCR A Extended")) +
  theme(plot.title    = element_text(size = 19),
        plot.subtitle = element_text(color = "gray60"),
        plot.caption  = element_text(color = "gray60"),
        legend.position = "none") +
  theme(axis.title.x = element_text(hjust = 0.36)) 

ggsave(file = "west_playoff_race.png", plot = west_playoff_race_plot, 
       dpi = 600, width = 30, height = 20, units = "cm", limitsize = FALSE)

