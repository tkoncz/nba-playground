library(data.table)
library(dplyr)

fun_create_game_list <- function(df_of_schedule, season_id, df_of_teams) {
  
  season <- df_of_schedule %>%
    filter(Season == "2017-2018")
  
  season <- season %>%
    mutate(Home.W = (Home.Points > Away.Points),
           Away.W = !Home.W)
  
  away_games <- season %>% 
    select(Date, Away.Team, Home.Team, Away.W) %>%
    rename("Team"     = "Away.Team") %>%
    rename("Opponent" = "Home.Team") %>%
    rename("W"        = "Away.W") %>%
    mutate("Home" = FALSE)
  
  home_games <- season %>% 
    select(Date, Home.Team, Away.Team, Home.W) %>%
    rename("Team"     = "Home.Team") %>%
    rename("Opponent" = "Away.Team") %>%
    rename("W"        = "Home.W") %>%
    mutate("Home" = TRUE)
  
  games <- rbind(away_games, home_games)
  
  games <- games %>%
    arrange(Team, Date) %>%
    group_by(Team) %>%
    mutate(Game.No   = row_number()) %>%
    mutate(Win.Total = cumsum(W)) %>%
    ungroup()
  
  games <- games %>%
    left_join(teams, by = "Team") %>%
    left_join(teams, by = c("Opponent" = "Team")) %>%
    rename("Conference"      = "Conference.x",
           "Division"        = "Division.x",
           "Opp. Conference" = "Conference.y",
           "Opp. Division"   = "Division.y") 
  
  return(games)
}

#########################################################################################################################
#########################################################################################################################

fun_create_winpct_matrices <- function(df_of_games) {
  
  max_GameNo <- df_of_games %>%
    group_by(Team) %>%
    summarize(max = max(Game.No)) %>%
    summarize(min(max)) %>%
    pull()
  
  winpct_by_opponent   <- vector("list", max_GameNo)
  winpct_by_division   <- vector("list", max_GameNo)
  winpct_by_conference <- vector("list", max_GameNo)
  
  for(i in c(1:max_GameNo)) {
    winpct_by_opponent[[i]] <- df_of_games %>%
      filter(Game.No <= i) %>%
      group_by(Team, Opponent) %>%
      summarize(Win.Pct = sum(W) / n()) %>%
      ungroup() %>%
      mutate(Game.No = i) %>%
      arrange(Team, Opponent)
    
    winpct_by_division[[i]] <- df_of_games %>%
      filter(Game.No <= i) %>%
      group_by(Team, `Opp. Division`) %>%
      summarize(Win.Pct = sum(W) / n()) %>%
      ungroup() %>%
      mutate(Game.No = i) %>%
      arrange(Team, `Opp. Division`)
    
    winpct_by_conference[[i]] <- df_of_games %>%
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
  
  winpct_list <- list(winpct_by_opponent, winpct_by_division, winpct_by_conference)
  return(winpct_list)
  
}

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

#########################################################################################################################
#########################################################################################################################

fun_base_standings <- function(df_of_games) {
  #calculates conference & division standings based on simple win.pct ranking, without any resolution for ties
  
  standings <- df_of_games %>%
    group_by(Conference, Game.No) %>%
    mutate(Conference.Rank = rank(-Win.Total, ties.method = "min")) %>%
    ungroup() %>%
    group_by(Conference, Game.No, Conference.Rank) %>%
    mutate(Tie.Count = n()) %>%
    ungroup()
  
  division_standings <- df_of_games %>%
    group_by(Game.No, Division) %>%
    mutate(Division.Rank = rank(-Win.Total, ties.method = "min")) %>%
    ungroup() %>%
    select(Game.No, Team, Division.Rank)
  
  standings <- standings %>%
    left_join(division_standings, by = c("Game.No", "Team"))
  
  
  standings <- standings %>%
    select(Game.No, Conference, Conference.Rank, Division.Rank, Division, Team, Win.Total, Tie.Count) %>%
    arrange(Game.No, Conference, Conference.Rank, Team)
  
  return(standings)
}

#########################################################################################################################
#########################################################################################################################


################################
#### RESOLVING TIE-BREAKERS ####
################################

# a. Two Teams Tied
fun_get_twoway_tie_breakers <- function(df_of_standings) {
  #calculates all ties among exactly 2 teams
  
  twoway_tie_breakers <- df_of_standings %>%
    filter(Tie.Count == 2) %>%
    arrange(Game.No, Conference, Conference.Rank) %>%
    select(Game.No, Conference, Conference.Rank, Team)
  
  return(twoway_tie_breakers)
}

fun_recalculate_standings <- function(df_of_standings, df_of_penalities) {
  #recalculates standings based on current standings + df containing "penalities", calculated from tie-breaker rules
  
  df_of_standings <- df_of_standings %>%
    left_join(df_of_penalities, by = c("Team", "Game.No")) %>%
    mutate(Conference.Rank = ifelse(!is.na(Conference.Rank.Addon),
                                    Conference.Rank + Conference.Rank.Addon,
                                    Conference.Rank)) %>%
    mutate(Conference.Rank = as.integer(Conference.Rank)) %>%
    select(everything(), -Conference.Rank.Addon) %>%
    group_by(Conference, Game.No, Conference.Rank) %>%
    mutate(Tie.Count = n()) %>%
    ungroup()
  
  return(df_of_standings)
}

# (1) Better winning percentage in games against each other
fun_tiebreaker_2way_rule1 <- function(df_of_standings, df_of_winpct_by_opponent) {
  #calculates tie-breaker "penalities" based on rule: "(1) Better winning percentage in games against each other"
  
  twoway_tie_breakers <- fun_get_twoway_tie_breakers(df_of_standings)
  
  tie_breaker_penalities <- twoway_tie_breakers %>%
    left_join(twoway_tie_breakers, by = c("Game.No", "Conference", "Conference.Rank")) %>%
    filter(Team.x != Team.y) %>%
    rename("Team"     = "Team.x") %>%
    rename("Opponent" = "Team.y") %>% 
    left_join(df_of_winpct_by_opponent, by = c("Team", "Opponent","Game.No")) %>%
    mutate(Conference.Rank.Addon = ifelse(!is.na(Win.Pct) & Win.Pct < 0.5, 1, 0)) %>%
    select(Team, Game.No, Conference.Rank.Addon)
  
  fun_recalculate_standings(df_of_standings = df_of_standings, df_of_penalities = tie_breaker_penalities)
}

# standings <- fun_tiebreaker_2way_rule1(standings, winpct_by_opponent)

# #test: game 70
# standings %>%
#   filter(Game.No == 70) %>%
#   filter(Conference == "Eastern") %>%
#   arrange(conference_rank_1)

# (2) Division winner (this criterion is applied regardless of whether the tied teams are in the same division).
fun_tiebreaker_2way_rule2 <- function(df_of_standings) {
  #calculates tie-breaker "penalities" based on rule: "(2) Division winner (this criterion is applied regardless of whether the tied teams are in the same division)."
  
  twoway_tie_breakers <- fun_get_twoway_tie_breakers(df_of_standings)
  twoway_tie_breakers <- twoway_tie_breakers %>%
    select(Game.No, Team)
  
  tie_breaker_penalities <- twoway_tie_breakers %>%
    left_join(df_of_standings, by = c("Game.No", "Team")) %>%
    group_by(Game.No, Conference, Conference.Rank) %>%
    mutate(Max.Division.Rank = max(Division.Rank),
           Min.Division.Rank = min(Division.Rank)) %>%
    ungroup() %>%
    mutate(Conference.Rank.Addon = ifelse(Min.Division.Rank == 1 & Max.Division.Rank != 1,
                                          ifelse(Division.Rank == 1, 0, 1),
                                          0)) %>%
    select(Game.No, Team, Conference.Rank.Addon)
  
  fun_recalculate_standings(df_of_standings = df_of_standings, df_of_penalities = tie_breaker_penalities)
}

# standings <- fun_tiebreaker_2way_rule2(standings)

# #test: game 28
# standings %>%
#   filter(Game.No == 28) %>%
#   filter(Conference == "Eastern") %>%
#   arrange(conference_rank_2)

# (3) Better winning percentage against teams in own division (only if tied teams are in same division).
fun_tiebreaker_2way_rule3 <- function(df_of_standings, df_of_winpct_by_division) {
  #calculates tie-breaker "penalities" based on rule: "(3) Better winning percentage against teams in own division (only if tied teams are in same division)."
  
  twoway_tie_breakers <- fun_get_twoway_tie_breakers(df_of_standings)
  twoway_tie_breakers <- twoway_tie_breakers %>%
    select(Game.No, Team)
  
  #Step 1: get list of ties within same division
  twoway_tie_breakers <- twoway_tie_breakers %>%
    left_join(df_of_standings, by = c("Game.No", "Team")) %>%
    group_by(Game.No, Conference, Conference.Rank) %>%
    mutate(Division.Count = n_distinct(Division)) %>%
    ungroup() %>%
    filter(Division.Count == 1) %>%
    select(Game.No, Conference, Division, Conference.Rank, Team) %>%
    left_join(df_of_winpct_by_division, by = c("Game.No" = "Game.No", "Team" = "Team", "Division" = "Opp. Division"))
  
  #Step2: add win.pct of tied team & rank-penalty based on that
  tie_breaker_penalities <- twoway_tie_breakers %>%
    left_join(twoway_tie_breakers, by = c("Game.No", "Division", "Conference.Rank")) %>%
    filter(Team.x != Team.y) %>%
    rename("Team"        = "Team.x",
           "Opponent"    = "Team.y",
           "Win.Pct"     = "Win.Pct.x",
           "Opp.Win.Pct" = "Win.Pct.y") %>%
    mutate(Conference.Rank.Addon = ifelse(!is.na(Win.Pct) & !is.na(Opp.Win.Pct), 
                                          ifelse(Win.Pct < Opp.Win.Pct, 1, 0),
                                          0)) %>%
    select(Game.No, Team, Conference.Rank.Addon)
  
  fun_recalculate_standings(df_of_standings = df_of_standings, df_of_penalities = tie_breaker_penalities)
}

# standings <- fun_tiebreaker_2way_rule3(standings, winpct_by_division)

# #TODO: create test
# standings %>%
#   filter(Game.No == 43)

# (4) Better winning percentage against teams in own conference.
fun_tiebreaker_2way_rule4 <- function(df_of_standings, df_of_winpct_by_conference) {
  #calculates tie-breaker "penalities" based on rule: "(4) Better winning percentage against teams in own conference."
  
  twoway_tie_breakers <- fun_get_twoway_tie_breakers(df_of_standings)
  
  # Step1: list of tied teams, enriched with conf. win %
  twoway_tie_breakers <- twoway_tie_breakers %>%
    left_join(df_of_winpct_by_conference, by = c("Game.No" = "Game.No", "Team" = "Team", "Conference" = "Opp. Conference")) %>%
    select(Game.No, Conference, Team, Conference.Rank, Win.Pct)
  
  # Step2: add win.pct of tied team & rank-penalty based on that
  tie_breaker_penalities <- twoway_tie_breakers %>%
    left_join(twoway_tie_breakers, by = c("Game.No", "Conference", "Conference.Rank")) %>%
    filter(Team.x != Team.y) %>%
    rename("Team"        = "Team.x",
           "Opponent"    = "Team.y",
           "Win.Pct"     = "Win.Pct.x",
           "Opp.Win.Pct" = "Win.Pct.y") %>%
    mutate(Conference.Rank.Addon = ifelse(!is.na(Win.Pct) & !is.na(Opp.Win.Pct), 
                                          ifelse(Win.Pct < Opp.Win.Pct, 1, 0),
                                          0)) %>%
    select(Game.No, Team, Conference.Rank.Addon)
  
  fun_recalculate_standings(df_of_standings = df_of_standings, df_of_penalities = tie_breaker_penalities)
}

# standings <- fun_tiebreaker_2way_rule4(standings, winpct_by_conference)

# standings %>% filter(tie_count == 2) %>% summarize(m = max(Game.No))
# twoway_tie_breakers %>% filter(Game.No == 39)

# (5) Better winning percentage against teams eligible for playoffs in own conference (including teams that finished the regular season tied for a playoff position).
# (6) Better winning percentage against teams eligible for playoffs in opposite conference (including teams that finished the regular season tied for a playoff position).
# (7) Better net result of total points scored less total points allowed against all opponents (“point differential”).


# b. More Than Two Teams Tied
fun_get_multiway_tie_breakers <- function(df_of_standings) {
  #calculates all ties among exactly 2 teams
  
  multiway_tie_breakers <- df_of_standings %>%
    filter(Tie.Count > 2) %>%
    arrange(Game.No, Conference, Conference.Rank)
  
  return(multiway_tie_breakers)
}

# (1) Division winner (this criterion is applied regardless of whether the tied teams are in the same division).
fun_tiebreaker_multiway_rule1 <- function(df_of_standings) {
  #calculates tie-breaker "penalities" based on rule: "(1) Division winner (this criterion is applied regardless of whether the tied teams are in the same division)."
  
  multiway_tie_breakers <- fun_get_multiway_tie_breakers(df_of_standings)
  
  #Step1: get best division rank among tied teams
  best_division_ranks <- multiway_tie_breakers %>%
    select(Game.No, Conference, Conference.Rank, Division.Rank) %>%
    group_by(Game.No, Conference, Conference.Rank) %>%
    summarize(First.Division.Rank = min(Division.Rank)) %>%
    ungroup() %>%
    mutate(First.Division.Rank = as.integer(First.Division.Rank)) %>%
    arrange(Game.No, Conference, Conference.Rank)
  
  #Step2: give penalty to not 1st placed teams, if there is a 1st places one among the group
  tie_breaker_penalities <- multiway_tie_breakers %>%
    select(Game.No, Conference, Conference.Rank, Division.Rank, Team) %>%
    left_join(best_division_ranks, by = c("Game.No", "Conference", "Conference.Rank")) %>%
    mutate(Conference.Rank.Addon = ifelse(First.Division.Rank == 1 & Division.Rank > 1, 1, 0)) %>%
    select(Game.No, Team, Conference.Rank.Addon)
  
  fun_recalculate_standings(df_of_standings = df_of_standings, df_of_penalities = tie_breaker_penalities)
}


# standings <- fun_tiebreaker_multiway_rule1(standings)


# #test, 3rd place
# standings %>%
#   filter(Conference == "Eastern" & Game.No == 8) %>%
#   arrange(Conference.Rank)


# (2) Better winning percentage in all games among the tied teams.
fun_tiebreaker_multiway_rule2 <- function(df_of_standings, df_of_games) {
  #calculates tie-breaker "penalities" based on rule: "(2) Better winning percentage in all games among the tied teams."  
  
  multiway_tie_breakers <- fun_get_multiway_tie_breakers(df_of_standings)
  
  #Step1: calculate winpct among tied teams
  multiway_ties <- multiway_tie_breakers %>%
    select(Game.No, Conference, Conference.Rank) %>%
    distinct()
  
  winpct_by_opp_group_list_length <-  multiway_ties %>% nrow()
  winpct_by_opp_group <- vector("list", winpct_by_opp_group_list_length)
  
  for(i in c(1:winpct_by_opp_group_list_length)) {
    
    gm         <- multiway_ties[i, ]$Game.No
    conf       <- multiway_ties[i, ]$Conference
    conf_rank  <- multiway_ties[i, ]$Conference.Rank
    teams      <- multiway_tie_breakers %>% 
      filter(Game.No           == gm) %>%
      filter(Conference        == conf) %>%
      filter(Conference.Rank   == conf_rank) %>%
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
    select(Game.No, Team, Conference, Conference.Rank) %>%
    left_join(winpct_by_opp_group, by = c("Game.No", "Team"))
  
  #Step2: penalty calculation based on win% among tied teams
  tie_breaker_penalities <- multiway_tie_breakers %>% 
    group_by(Game.No, Conference, Conference.Rank) %>%
    mutate(Win.Pct.NA.check = is.na(sum(Win.Pct, na.rm = F))) %>%
    ungroup() %>%
    mutate(Win.Pct.Adj = ifelse(Win.Pct.NA.check, 0, Win.Pct)) %>%
    group_by(Game.No, Conference, Conference.Rank) %>%
    mutate(in.Group.Rank = dense_rank(-Win.Pct.Adj)) %>% 
    ungroup() %>%
    mutate(Conference.Rank.Addon = in.Group.Rank - 1) %>%
    select(Game.No, Team, Conference.Rank.Addon)
  
  fun_recalculate_standings(df_of_standings = df_of_standings, df_of_penalities = tie_breaker_penalities)
}


# standings <- fun_tiebreaker_multiway_rule2(standings, games)

# #test: Game 8, West
# standings %>%
#   filter(Game.No == 8 & Conference == "Western") %>%
#   arrange(Conference.Rank)

# (3) Better winning percentage against teams in own division (only if all tied teams are in same division).
fun_tiebreaker_multiway_rule3 <- function(df_of_standings, df_of_games) {
  #calculates tie-breaker "penalities" based on rule: "(3) Better winning percentage against teams in own division (only if all tied teams are in same division)."  
  
  multiway_tie_breakers <- fun_get_multiway_tie_breakers(df_of_standings)
  multiway_tie_breakers <- multiway_tie_breakers %>%
    group_by(Game.No, Conference, Conference.Rank) %>%
    mutate(Division.Count = n_distinct(Division)) %>%
    ungroup() %>%
    filter(Division.Count == 1) %>% 
    select(Game.No, Conference, Division, Conference.Rank, Team) 
  
  #Step: add win.pct of tied teams, then check which win.pct is higher, add rank-penalty based on that
  tie_breaker_penalities <- multiway_tie_breakers %>%
    left_join(winpct_by_division, by = c("Game.No" = "Game.No", "Team" = "Team", "Division" = "Opp. Division"))  %>%
    group_by(Game.No, Division, Conference.Rank) %>%
    mutate(Win.Pct.NA.check = is.na(sum(Win.Pct, na.rm = F))) %>%
    ungroup() %>%
    mutate(Win.Pct.Adj = ifelse(Win.Pct.NA.check, 0, Win.Pct)) %>%
    group_by(Game.No, Conference, Conference.Rank) %>%
    mutate(in.Division.Rank = dense_rank(-Win.Pct.Adj)) %>% 
    ungroup() %>%
    mutate(Conference.Rank.Addon = in.Division.Rank - 1) %>%
    select(Game.No, Team, Conference.Rank.Addon)
  
  updated_standings <- fun_recalculate_standings(df_of_standings = df_of_standings, df_of_penalities = tie_breaker_penalities)
  return(updated_standings)
}


# standings <- fun_tiebreaker_multiway_rule3(standings, games)

# #TODO: create test
# standings %>%
#   filter(Game.No == 17) %>%
#   filter(Conference == "Western") %>%
#   arrange(Conference.Rank)
# 
# multiway_tie_breakers_3 %>%
#   filter(Game.No == 17)

# (4) Better winning percentage against teams in own conference.
fun_tiebreaker_multiway_rule4 <- function(df_of_standings, df_of_winpct_by_conference) {
  #calculates tie-breaker "penalities" based on rule: "(4) Better winning percentage against teams in own conference."  
  
  multiway_tie_breakers <- fun_get_multiway_tie_breakers(df_of_standings)
  
  multiway_tie_breakers <- multiway_tie_breakers %>%
    select(Game.No, Conference, Conference.Rank, Team) %>%
    left_join(df_of_winpct_by_conference, by = c("Game.No" = "Game.No", "Team" = "Team", "Conference" = "Opp. Conference")) %>%
    select(Game.No, Conference, Team, Conference.Rank, Win.Pct)
  
  #Step
  tie_breaker_penalities <- multiway_tie_breakers %>%
    group_by(Game.No, Conference, Conference.Rank) %>%
    mutate(Win.Pct.NA.check = is.na(sum(Win.Pct, na.rm = F))) %>%
    ungroup() %>%
    mutate(Win.Pct.Adj = ifelse(Win.Pct.NA.check, 0, Win.Pct)) %>%
    group_by(Game.No, Conference, Conference.Rank) %>%
    mutate(in.Conference.Rank = dense_rank(-Win.Pct.Adj)) %>% 
    ungroup() %>%
    mutate(Conference.Rank.Addon = in.Conference.Rank - 1) %>%
    select(Game.No, Team, Conference.Rank.Addon)
  
  updated_standings <- fun_recalculate_standings(df_of_standings = df_of_standings, df_of_penalities = tie_breaker_penalities)
  return(updated_standings)
}


# standings <- fun_tiebreaker_multiway_rule4(standings, winpct_by_conference)

# #TODO: create test
# standings %>%
#   filter(Game.No == 5) %>%
#   filter(Conference == "Eastern") %>%
#   arrange(Conference.Rank)
# 
# multiway_tie_breakers_4 %>%
#   filter(Game.No == 5)

# (5) Better winning percentage against teams eligible for playoffs in own conference (including teams that finished the regular season tied for a playoff position).
# (6) Better net result of total points scored less total points allowed against all opponents (“point differential”).





fun_tiebreaker_2way     <- function(df_of_standings, df_of_winpct_by_opponent, df_of_winpct_by_division, df_of_winpct_by_conference) {
  updated_standings <- df_of_standings %>% 
    fun_tiebreaker_2way_rule1(df_of_winpct_by_opponent) %>%
    fun_tiebreaker_2way_rule2() %>%
    fun_tiebreaker_2way_rule3(df_of_winpct_by_division) %>%
    fun_tiebreaker_2way_rule4(df_of_winpct_by_conference)
  
  return(updated_standings)
}


fun_tiebreaker_multiway <- function(df_of_standings, df_of_games, df_of_winpct_by_conference) {
  updated_standings <- df_of_standings %>% 
    fun_tiebreaker_multiway_rule1() %>%
    fun_tiebreaker_multiway_rule2(df_of_games) %>%
    fun_tiebreaker_multiway_rule3(df_of_games) %>%
    fun_tiebreaker_multiway_rule4(df_of_winpct_by_conference)
  
  return(updated_standings)
}


fun_create_standings    <- function(df_of_games, df_of_winpct_by_opponent, df_of_winpct_by_division, df_of_winpct_by_conference) {
  
  standings <- fun_base_standings(df_of_games)
  standings <- fun_tiebreaker_2way(df_of_standings             = standings,
                                   df_of_winpct_by_opponent    = winpct_by_opponent,
                                   df_of_winpct_by_division    = winpct_by_division,
                                   df_of_winpct_by_conference  = winpct_by_conference) 
  
  standings <- fun_tiebreaker_multiway(df_of_standings             = standings,
                                       df_of_games                 = df_of_games,
                                       df_of_winpct_by_conference  = winpct_by_conference) 
  
  #calling 2-way resolution one more team, to create new 2-ties from breaking up multis
  standings <- fun_tiebreaker_2way(df_of_standings             = standings,
                                   df_of_winpct_by_opponent    = winpct_by_opponent,
                                   df_of_winpct_by_division    = winpct_by_division,
                                   df_of_winpct_by_conference  = winpct_by_conference) 
}

#standings %>% filter(Game.No > 40) %>% filter(Tie.Count > 1)
#TODO: Game 62, 70, SAS vs NOP