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

#note: could add opp. here if needed
away_games <- s_2018 %>% 
                select(Date, Away.Team, Away.W) %>%
                rename("Team" = "Away.Team") %>%
                rename("W"    = "Away.W") %>%
                mutate("Home" = FALSE)

home_games <- s_2018 %>% 
                select(Date, Home.Team, Home.W) %>%
                rename("Team" = "Home.Team") %>%
                rename("W"    = "Home.W") %>%
                mutate("Home" = TRUE)

games <- rbind(away_games, home_games) 
               
games <- games %>%
            arrange(Team, Date) %>%
            group_by(Team) %>%
            mutate(Game.No   = row_number()) %>%
            mutate(Win.Total = cumsum(W)) %>%
            ungroup()

west_teams <- c("Golden State Warriors",
                "Los Angeles Clippers",
                "Los Angeles Lakers",
                "Phoenix Suns",
                "Sacramento Kings",
                "Dallas Mavericks",
                "Houston Rockets",
                "Memphis Grizzlies",
                "New Orleans Pelicans",
                "San Antonio Spurs",
                "Denver Nuggets",
                "Minnesota Timberwolves",
                "Oklahoma City Thunder",
                "Portland Trail Blazers",
                "Utah Jazz")

games %>%
  filter(Team %in% west_teams) %>%
  ggplot(aes(x= Game.No, y= Win.Total, color = Team)) +
  geom_line() +
  theme_minimal()

west <- games %>%
          filter(Team %in% west_teams) %>%
          group_by(Game.No) %>%
          mutate(position = rank(-Win.Total, ties.method = 'first'))

west %>%
  filter(Game.No >= 42) %>%
  filter(Game.No <= 78) %>%
  filter(Team %in% c("Los Angeles Clippers",
         "New Orleans Pelicans",
         "San Antonio Spurs",
         "Denver Nuggets",
         "Minnesota Timberwolves",
         "Oklahoma City Thunder",
         "Portland Trail Blazers",
         "Utah Jazz")) %>%
  ggplot(aes(x= Game.No, y= position, color = Team)) + 
    geom_line(size = 1.05) +
    geom_point(size = 2) +
    scale_y_reverse() +
    theme_minimal()

