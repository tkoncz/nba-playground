library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)

rm(list = ls())
schedule <- fread(file = "schedulesTable.csv") 

schedule <- schedule %>%
              setnames(old = 1:6, new = c("Date", "Start_ET", "Away.Team", "Away.Points", "Home.Team", "Home.Points")) %>%
              select(Date, Start_ET, Away.Team, Away.Points, Home.Team, Home.Points)

##TODO: based on "Playoff" row extract last day of regular season

s_2018 <- schedule %>%
            filter(Date != "Playoffs") %>%
            tidyr::separate(col = Date, into = c("Day.Of.Week", "Month.Day", "Year"), sep = ", ", fill = "right") %>%
            mutate(Date = paste(Year, Month.Day, sep = " ")) %>%
            mutate(Date = ymd(Date)) %>%
            filter(Date >= date("2017-10-17")) %>%
            mutate(Away.Points = as.integer(Away.Points)) %>%
            mutate(Home.Points = as.integer(Home.Points)) %>%
            filter(!is.na(Away.Points)) %>%
            filter(!is.na(Home.Points)) %>%
            select(Date, Away.Team, Away.Points, Home.Team, Home.Points)

  

s_2018 <- s_2018 %>%
            mutate(Home.W = (Home.Points > Away.Points),
                   Away.W = !Home.W)


away_games <- s_2018 %>% 
                select(Date, Away.Team, Home.Team, Away.W) %>%
                rename("Team" = "Away.Team") %>%
                rename("Opponent" = "Home.Team") %>%
                rename("W"    = "Away.W") %>%
                mutate("Home" = FALSE)

home_games <- s_2018 %>% 
                select(Date, Home.Team, Away.Team, Home.W) %>%
                rename("Team" = "Home.Team") %>%
                rename("Opponent" = "Away.Team") %>%
                rename("W"    = "Home.W") %>%
                mutate("Home" = TRUE)

games <- rbind(away_games, home_games) 

games <- games %>%
            arrange(Team, Date) %>%
            group_by(Team) %>%
            mutate(Game.No   = row_number()) %>%
            mutate(Win.Total = cumsum(W)) %>%
            ungroup()

#### add Conference / Division columns
teams <- fread(file = "C:/Users/tkonc/Documents/Data/NBA Playground/Conference_Division_Team.csv") 

games <- games %>%
           left_join(teams, by = "Team")


## Game.No, Opponent matrix
max_GameNo <- games %>%
                group_by(Team) %>%
                summarize(max = max(Game.No)) %>%
                summarize(min(max)) %>%
                pull()

games_by_opponent <- vector("list", max_GameNo)

for(i in c(1:max_GameNo)) {
  games_by_opponent[[i]] <- games %>%
                              filter(Game.No <= i) %>%
                              group_by(Team, Opponent) %>%
                              summarize(Win.Pct = sum(W) / n()) %>%
                              mutate(Game.No = i) %>%
                              arrange(Team, Opponent) %>%
                              tidyr::spread(key = Opponent, value = Win.Pct)
}

games_by_opponent <- do.call(rbind, games_by_opponent)
## 


games %>%
  filter(Game.No <= 70) %>%
  group_by(Team, Opponent) %>%
  summarize(Win.Pct = sum(W) / n()) %>%
  filter(Team    == "Oklahoma City Thunder")

games %>%
  arrange(Game.No, Team, Opponent) %>%
  group_by(Team, Opponent) %>%
  (Win.Pct = cumsum(W)) %>%
  filter(Game.No == 70) %>%
  filter(Team    == "Oklahoma City Thunder")

games %>% head()

# games %>%
#   filter(Team %in% west_teams) %>%
#   ggplot(aes(x= Game.No, y= Win.Total, color = Team)) +
#   geom_line() +
#   theme_minimal()

standings <- games %>%
               group_by(Conference, Game.No) %>%
               mutate(rank_wins = rank(-Win.Total, ties.method = "min")) %>%
               ungroup()

standings <- standings %>%
               group_by(Conference, Game.No, rank_wins) %>%
               mutate(n = n()) %>%
               ungroup()

standings %>%
  filter(Conference == "Western") %>%
  filter(Game.No == 70) %>%
  arrange(rank_wins)

standings %>%
  filter(Game.No >= 42) %>%
  filter(Game.No <= 78) %>%
  # filter(Team %in% c("Los Angeles Clippers",
  #        "New Orleans Pelicans",
  #        "San Antonio Spurs",
  #        "Denver Nuggets",
  #        "Minnesota Timberwolves",
  #        "Oklahoma City Thunder",
  #        "Portland Trail Blazers",
  #        "Utah Jazz")) %>%
  ggplot(aes(x= Game.No, y= position, color = Team)) + 
    geom_line(size = 1.05) +
    geom_point(size = 2) +
    scale_y_reverse() +
    facet_grid(~Conference) +
    theme_minimal()

