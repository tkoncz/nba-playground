library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)

rm(list = ls())
schedule <- fread(file = "schedulesTable.csv") 

schedule <- schedule %>%
              setnames(old = 1:6, new = c("Date", "Start_ET", "Away.Team", "Away.Points", "Home.Team", "Home.Points")) %>%
              select(Date, Start_ET, Away.Team, Away.Points, Home.Team, Home.Points)

##TODO: based on "Playoff" row extract last day of regular season. 
##TODO: also, add a "season" column, and regular vs PO season

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

### TODO: all above could be moved to the schedule extraction code piece

away_games <- s_2018 %>% 
                select(Date, Away.Team, Home.Team, Away.W) %>%
                rename("Team"     = "Away.Team") %>%
                rename("Opponent" = "Home.Team") %>%
                rename("W"        = "Away.W") %>%
                mutate("Home" = FALSE)

home_games <- s_2018 %>% 
                select(Date, Home.Team, Away.Team, Home.W) %>%
                rename("Team"     = "Home.Team") %>%
                rename("Opponent" = "Away.Team") %>%
                rename("W"        = "Home.W") %>%
                mutate("Home" = TRUE)

games <- rbind(away_games, home_games) 


##TODO: remove Game.No filter once all games are played!!!
games <- games %>%
            arrange(Team, Date) %>%
            group_by(Team) %>%
            mutate(Game.No   = row_number()) %>%
            filter(Game.No <= 78) %>%
            mutate(Win.Total = cumsum(W)) %>%
            ungroup()


## add Conference / Division columns for Team & Opponent
teams <- fread(file = "C:/Users/tkonc/Documents/Data/NBA Playground/Conference_Division_Team.csv") 

games <- games %>%
           left_join(teams, by = "Team") %>%
           left_join(teams, by = c("Opponent" = "Team")) %>%
           rename("Conference"      = "Conference.x",
                  "Division"        = "Division.x",
                  "Opp. Conference" = "Conference.y",
                  "Opp. Division"   = "Division.y") 
## 


## Game.No, Opponent matrix
max_GameNo <- games %>%
                group_by(Team) %>%
                summarize(max = max(Game.No)) %>%
                summarize(min(max)) %>%
                pull()

winpct_by_opponent   <- vector("list", max_GameNo)
winpct_by_division   <- vector("list", max_GameNo)
winpct_by_conference <- vector("list", max_GameNo)

for(i in c(1:max_GameNo)) {
  winpct_by_opponent[[i]]   <- games %>%
                                filter(Game.No <= i) %>%
                                group_by(Team, Opponent) %>%
                                summarize(Win.Pct = sum(W) / n()) %>%
                                ungroup() %>%
                                mutate(Game.No = i) %>%
                                arrange(Team, Opponent) #%>%
                                # tidyr::spread(key = Opponent, value = Win.Pct)
  
  winpct_by_division[[i]]   <- games %>%
                                 filter(Game.No <= i) %>%
                                 group_by(Team, `Opp. Division`) %>%
                                 summarize(Win.Pct = sum(W) / n()) %>%
                                 ungroup() %>%
                                 mutate(Game.No = i) %>%
                                 arrange(Team, `Opp. Division`)
  
  winpct_by_conference[[i]] <- games %>%
                                 filter(Game.No <= i) %>%
                                 group_by(Team, `Opp. Conference`) %>%
                                 summarize(Win.Pct = sum(W) / n()) %>%
                                 ungroup() %>%
                                 mutate(Game.No = i) %>%
                                 arrange(Team, `Opp. Conference`)
}

winpct_by_opponent   <- do.call(rbind, winpct_by_opponent)
winpct_by_division   <- do.call(rbind, winpct_by_division)
winpct_by_conference <- do.call(rbind, winpct_by_conference)

# winpct_by_opponent %>%
#   filter(Game.No == 78) %>%
#   filter(Team == "San Antonio Spurs") %>%
#   filter(Opponent == "Minnesota Timberwolves")

# test for winpct_by_division: Game no 55, Boston Celtics
# games %>%
#   filter(Game.No <= 55) %>%
#   group_by(Team, `Opp. Division`) %>%
#   summarize(Win.Pct = sum(W) / n()) %>%
#   ungroup() %>%
#   mutate(Game.No = i) %>%
#   arrange(Team, `Opp. Division`) %>%
#   filter(Team == "Boston Celtics")
# 
# winpct_by_division %>%
#   filter(Game.No == 55) %>%
#   filter(Team == "Boston Celtics")
## 


standings <- games %>%
               group_by(Conference, Game.No) %>%
               mutate(conference_rank_0 = rank(-Win.Total, ties.method = "min")) %>%
               ungroup() %>%
               group_by(Conference, Game.No, conference_rank_0) %>%
               mutate(tie_count = n()) %>%
               ungroup()

division_standings <- games %>%
                        group_by(Game.No, Division) %>%
                        mutate(division_rank = rank(-Win.Total, ties.method = "min")) %>%
                        ungroup() %>%
                        select(Game.No, Team, division_rank)
                        # select(Game.No, Division, division_rank, Team, Win.Total) %>%
                        # arrange(desc(Game.No), Division, division_rank)

standings <- standings %>%
               left_join(division_standings, by = c("Game.No", "Team"))


standings <- standings %>%
               select(Game.No, Conference, Division, Team, Win.Total, conference_rank_0, tie_count, division_rank)

################################
#### RESOLVING TIE-BREAKERS ####
################################

# a. Two Teams Tied
twoway_tie_breakers <- standings %>%
                         filter(tie_count == 2) %>%
                         arrange(conference_rank_0) %>%
                         select(Conference, Game.No, conference_rank_0, Team)

# (1) Better winning percentage in games against each other
twoway_tie_breakers_1 <- twoway_tie_breakers %>%
                           left_join(twoway_tie_breakers, by = c("Conference", "Game.No", "conference_rank_0")) %>%
                           filter(Team.x != Team.y) %>%
                           rename("Team"     = "Team.x") %>%
                           rename("Opponent" = "Team.y") %>% 
                           left_join(winpct_by_opponent, by = c("Team", "Opponent","Game.No")) %>%
                           mutate(conference_rank_1_addon = ifelse(!is.na(Win.Pct) & Win.Pct < 0.5, 1, 0)) %>%
                           select(Team, Game.No, conference_rank_1_addon)

standings <- standings %>%
               left_join(twoway_tie_breakers_1, by = c("Team", "Game.No")) %>%
               mutate(conference_rank_1 = ifelse(!is.na(conference_rank_1_addon),
                                                 conference_rank_0 + conference_rank_1_addon,
                                                 conference_rank_0)) %>%
               mutate(conference_rank_1 = as.integer(conference_rank_1)) %>%
               select(everything(), -conference_rank_0, -conference_rank_1_addon)

standings <- standings %>%
               group_by(Conference, Game.No, conference_rank_1) %>%
               mutate(tie_count = n()) %>%
               ungroup()

# #test: game 70
# standings %>%
#   filter(Game.No == 70) %>%
#   filter(Conference == "Eastern") %>%
#   arrange(conference_rank_1)

# (2) Division winner (this criterion is applied regardless of whether the tied teams are in the same division).
twoway_tie_breakers_2 <- standings %>%
                           filter(tie_count == 2) %>%
                           arrange(conference_rank_1) %>%
                           group_by(Game.No, Conference, Win.Total) %>%
                           mutate(max_division_rank = max(division_rank),
                                  min_division_rank = min(division_rank)) %>%
                           ungroup() %>%
                           mutate(conference_rank_2_addon = ifelse(min_division_rank == 1 & max_division_rank != 1,
                                                                   ifelse(division_rank == 1, 0, 1),
                                                                   0)) %>%
                           select(Game.No, Team, conference_rank_2_addon)

standings <- standings %>%
               left_join(twoway_tie_breakers_2, by = c("Game.No", "Team")) %>%
               mutate(conference_rank_2 = ifelse(!is.na(conference_rank_2_addon),
                                                 conference_rank_1 + conference_rank_2_addon,
                                                 conference_rank_1)) %>%
               mutate(conference_rank_2 = as.integer(conference_rank_2)) %>%
               select(everything(), -conference_rank_1, -conference_rank_2_addon)

standings <- standings %>%
               group_by(Conference, Game.No, conference_rank_2) %>%
               mutate(tie_count = n()) %>%
               ungroup()

# #test: game 28
# standings %>%
#   filter(Game.No == 28) %>%
#   filter(Conference == "Eastern") %>%
#   arrange(conference_rank_2)

# (3) Better winning percentage against teams in own division (only if tied teams are in same division).
#Step 1: get list of ties within same division
twoway_tie_breakers <- standings %>%
                         filter(tie_count == 2) %>%
                         group_by(Game.No, Conference, conference_rank_2) %>%
                         mutate(division_count = n_distinct(Division)) %>%
                         ungroup() %>%
                         filter(division_count == 1) %>%
                         select(Game.No, Conference, Division, conference_rank_2, Team) %>%
                         left_join(winpct_by_division, by = c("Game.No" = "Game.No", "Team" = "Team", "Division" = "Opp. Division"))

#Step2: add win.pct of tied team
twoway_tie_breakers_3 <-  twoway_tie_breakers %>%
                            left_join(twoway_tie_breakers, by = c("Game.No", "Division", "conference_rank_2")) %>%
                            filter(Team.x != Team.y) %>%
                            rename("Team"     = "Team.x",
                                   "Opponent" = "Team.y",
                                   "Win.Pct" = "Win.Pct.x",
                                   "Opp.Win.Pct" = "Win.Pct.y") %>%
                            mutate(conference_rank_3_addon = ifelse(!is.na(Win.Pct) & !is.na(Opp.Win.Pct), 
                                                                    ifelse(Win.Pct < Opp.Win.Pct, 1, 0),
                                                                    0)) %>%
                            select(Game.No, Team, conference_rank_3_addon)

#Step3: check which win.pct is higher, add rank-penalty based on that
standings <- standings %>%
               left_join(twoway_tie_breakers_3, by = c("Game.No", "Team")) %>%
               mutate(conference_rank_3 = ifelse(!is.na(conference_rank_3_addon),
                                                 conference_rank_2 + conference_rank_3_addon,
                                                 conference_rank_2)) %>%
               select(everything(), -conference_rank_2, -conference_rank_3_addon)

#Step4: recalculate ties
standings <- standings %>%
              group_by(Conference, Game.No, conference_rank_3) %>%
              mutate(tie_count = n()) %>%
              ungroup()

# #TODO: create test
# standings %>%
#   filter(Game.No == 43) %>%
#   filter(tie_count == 2)
#
# twoway_tie_breakers_3 %>% filter(Game.No == 43)

# (4) Better winning percentage against teams in own conference.
# Step1: list of tied teams, enriched with conf. win %
twoway_tie_breakers <- standings %>%
                         filter(tie_count == 2) %>%
                         left_join(winpct_by_conference, by = c("Game.No" = "Game.No", "Team" = "Team", "Conference" = "Opp. Conference")) %>%
                         select(Game.No, Conference, Team, conference_rank_3, Win.Pct)

#Step2: add win.pct of tied team
twoway_tie_breakers_4 <-  twoway_tie_breakers %>%
                            left_join(twoway_tie_breakers, by = c("Game.No", "Conference", "conference_rank_3")) %>%
                            filter(Team.x != Team.y) %>%
                            rename("Team"     = "Team.x",
                                   "Opponent" = "Team.y",
                                   "Win.Pct" = "Win.Pct.x",
                                   "Opp.Win.Pct" = "Win.Pct.y") %>%
                            mutate(conference_rank_4_addon = ifelse(!is.na(Win.Pct) & !is.na(Opp.Win.Pct), 
                                                                    ifelse(Win.Pct < Opp.Win.Pct, 1, 0),
                                                                    0)) %>%
                            select(Game.No, Team, conference_rank_4_addon)

#Step3: check which win.pct is higher, add rank-penalty based on that
standings <- standings %>%
               left_join(twoway_tie_breakers_4, by = c("Game.No", "Team")) %>%
               mutate(conference_rank_4 = ifelse(!is.na(conference_rank_4_addon),
                                                 conference_rank_3 + conference_rank_4_addon,
                                                 conference_rank_3)) %>%
               select(everything(), -conference_rank_3, -conference_rank_4_addon)

#Step4: recalculate ties
standings <- standings %>%
               group_by(Conference, Game.No, conference_rank_4) %>%
               mutate(tie_count = n()) %>%
               ungroup()

# standings %>% filter(tie_count == 2) %>% summarize(m = max(Game.No))
# twoway_tie_breakers %>% filter(Game.No == 39)

# (5) Better winning percentage against teams eligible for playoffs in own conference (including teams that finished the regular season tied for a playoff position).
# (6) Better winning percentage against teams eligible for playoffs in opposite conference (including teams that finished the regular season tied for a playoff position).
# (7) Better net result of total points scored less total points allowed against all opponents (“point differential”).
# 
standings <- standings %>%
               rename("conference_rank" = "conference_rank_4")

# b. More Than Two Teams Tied
multiway_tie_breakers <- standings %>%
                           filter(tie_count > 2) %>%
                           arrange(conference_rank) %>%
                           select(Conference, Game.No, conference_rank, Division, division_rank, Team)

# (1) Division winner (this criterion is applied regardless of whether the tied teams are in the same division).
#Step1: get best division rank among tied teams
best_division_ranks <- multiway_tie_breakers %>%
                         group_by(Game.No, Conference, conference_rank) %>%
                         summarize(first_division_rank = min(division_rank)) %>%
                         ungroup() %>%
                         arrange(Game.No, Conference, conference_rank)

#Step2: give penalty to not 1st placed teams, if there is a 1st places one among the group
multiway_tie_breakers_1 <- multiway_tie_breakers %>%
                             left_join(best_division_ranks, by = c("Game.No", "Conference", "conference_rank")) %>%
                             mutate(conference_rank_1_addon = ifelse(first_division_rank == 1 & division_rank > 1, 1, 0)) %>%
                             select(Game.No, Team, conference_rank_1_addon)

#Step3: recalculate rankings with new penalty
standings <- standings %>%
               left_join(multiway_tie_breakers_1, by = c("Game.No", "Team")) %>%
               mutate(conference_rank_1 = ifelse(!is.na(conference_rank_1_addon),
                                                 conference_rank + conference_rank_1_addon,
                                                 conference_rank)) %>%
               select(everything(), -conference_rank, -conference_rank_1_addon)  


standings <- standings %>%
               group_by(Conference, Game.No, conference_rank_1) %>%
               mutate(tie_count = n()) %>%
               ungroup()

# #test, 3rd place
# standings %>%
#   filter(Conference == "Eastern" & Game.No == 8) %>%
#   arrange(conference_rank_1)


# (2) Better winning percentage in all games among the tied teams.
#Step1: get all multiway ties
multiway_tie_breakers <- standings %>%
                           filter(tie_count > 2) %>%
                           arrange(conference_rank_1) %>%
                           select(Game.No, Conference, conference_rank_1, Team)

#Step2: calculate winpct among tied teams
multiway_ties <- multiway_tie_breakers %>%
                   select(Game.No, Conference, conference_rank_1) %>%
                   distinct()

winpct_by_opp_group_list_length <-  multiway_ties %>% nrow()
winpct_by_opp_group <- vector("list", winpct_by_opp_group_list_length)

for(i in c(1:winpct_by_opp_group_list_length)) {
  
  gm         <- multiway_ties[i, ]$Game.No
  conf       <- multiway_ties[i, ]$Conference
  conf_rank  <- multiway_ties[i, ]$conference_rank_1
  teams      <- multiway_tie_breakers %>% 
                  filter(Game.No           == gm) %>%
                  filter(Conference        == conf) %>%
                  filter(conference_rank_1 == conf_rank) %>%
                  pull(Team)
  
  winpct_by_opp_group[[i]] <- games %>%
                                filter(Game.No    <=   gm) %>%
                                filter(Conference ==   conf) %>%
                                filter(Team       %in% teams) %>%
                                filter(Opponent   %in% teams) %>%
                                group_by(Team) %>%
                                summarize(Win.Pct = sum(W) / n()) %>%
                                ungroup() %>%
                                mutate(Game.No = gm)

}

winpct_by_opp_group <- do.call(rbind, winpct_by_opp_group)

multiway_tie_breakers <- multiway_tie_breakers %>%
                           left_join(winpct_by_opp_group, by = c("Game.No", "Team"))

#Step3: penalty calculation based on win% among tied teams
multiway_tie_breakers_2 <- multiway_tie_breakers %>% 
                             group_by(Game.No, Conference, conference_rank_1) %>%
                             mutate(Win.Pct.NA.check = is.na(sum(Win.Pct, na.rm = F))) %>%
                             ungroup() %>%
                             mutate(Win.Pct.Adj = ifelse(Win.Pct.NA.check, 0, Win.Pct)) %>%
                             group_by(Game.No, Conference, conference_rank_1) %>%
                             mutate(ingroup_rank = dense_rank(-Win.Pct.Adj)) %>% 
                             ungroup() %>%
                             mutate(conference_rank_2_addon = ingroup_rank - 1) %>%
                             select(Game.No, Team, conference_rank_2_addon)

#Step4: recalculate rankings with new penalty
standings <- standings %>%
               left_join(multiway_tie_breakers_2, by = c("Game.No", "Team")) %>%
               mutate(conference_rank_2 = ifelse(!is.na(conference_rank_2_addon),
                                                 conference_rank_1 + conference_rank_2_addon,
                                                 conference_rank_1)) %>%
               select(everything(), -conference_rank_1, -conference_rank_2_addon)  


standings <- standings %>%
               group_by(Conference, Game.No, conference_rank_2) %>%
               mutate(tie_count = n()) %>%
               ungroup()

# #test: Game 8, West
# standings %>%
#   filter(Game.No == 8 & Conference == "Western") %>%
#   arrange(conference_rank_2)

# (3) Better winning percentage against teams in own division (only if all tied teams are in same division).
# (4) Better winning percentage against teams in own conference.
# (5) Better winning percentage against teams eligible for playoffs in own conference (including teams that finished the regular season tied for a playoff position).
# (6) Better net result of total points scored less total points allowed against all opponents (“point differential”).






## VISUALS

# games %>%
#   filter(Team %in% west_teams) %>%
#   ggplot(aes(x= Game.No, y= Win.Total, color = Team)) +
#   geom_line() +
#   theme_minimal()

# 
# standings %>%
#   filter(Game.No >= 42) %>%
#   filter(Game.No <= 78) %>%
#   # filter(Team %in% c("Los Angeles Clippers",
#   #        "New Orleans Pelicans",
#   #        "San Antonio Spurs",
#   #        "Denver Nuggets",
#   #        "Minnesota Timberwolves",
#   #        "Oklahoma City Thunder",
#   #        "Portland Trail Blazers",
#   #        "Utah Jazz")) %>%
#   ggplot(aes(x= Game.No, y= position, color = Team)) + 
#     geom_line(size = 1.05) +
#     geom_point(size = 2) +
#     scale_y_reverse() +
#     facet_grid(~Conference) +
#     theme_minimal()

