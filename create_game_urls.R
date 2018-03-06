library(data.table)
library(dplyr)
library(rvest)

#from original date format to R date
fun_date_2_dateKey <- function(date_String) {
  date <- substr(date_String, 6, nchar(date_String))
  date <- as.Date(date, format = "%B %d, %Y")
  return(date)
}

#creates game url based on dt columns
fun_create_game_url <- function(Date_Key, TeamCode) {
  base_url <- "https://www.basketball-reference.com/boxscores/"
  url <- paste(base_url, Date_Key, "0", TeamCode, ".html", sep= "")
  return(url)
}

########################################################################################
############################################

dt_Schedule <- fread("schedulesTable.csv")
dt_Schedule <- dt_Schedule %>%
                setnames(old = 1:6, new = c("Date", "Start_ET", "Away.Team", "Away.Points", "Home.Team", "Home.Points")) %>%
                select(everything(), -V7, -V8, -Notes) %>%
                filter(Start_ET != "Playoffs")

dt_teamNameMapping <- fread("teamNameMapping.csv")
dt_teamNameMapping <- dt_teamNameMapping %>%
                        setnames(old = "Team.Name", new = "Home.Team")

dt_Schedule <- dt_Schedule %>%
                left_join(dt_teamNameMapping, by = "Home.Team") %>%
                mutate(Date = date_2_dateKey(Date)) %>%
                mutate(Date.Key = format(Date, "%Y%m%d")) %>%
                mutate(Game.URL = create_game_url(as.character(Date.Key), Team.Code))

fun_get_basic_box_score <- function(df, sHome_or_Away) {
  header.Row <- df[1, -1] %>%
    t()
  rownames(header.Row) <- c()
  header.Row <- rbind("Players", header.Row)
  
  basic_box_score_stats <- df %>%
    setnames(header.Row) %>%
    filter(!Players %in% c("Starters", "Reserves")) %>%
    mutate(Starter.Reserve = ifelse(row_number() <= 5, "Starter", "Reserve")) %>%
    mutate(Home.Away = sHome_or_Away)

  return(basic_box_score_stats)
}


fun_get_game_box_score <- function(game_url) {
  html <- read_html(game_url)
  node <- html_nodes(html, "table")
  box_score_tables <- html_table(node, header = T)
  
  # away - basic
  away_basic_box_score_stats <- fun_get_basic_box_score(box_score_tables[[1]], "Away")
  home_basic_box_score_stats <- fun_get_basic_box_score(box_score_tables[[3]], "Home")
  
  # away - advanced
  header.Row <- box_score_tables[[2]][1, -1] %>%
    t()
  rownames(header.Row) <- c()
  header.Row <- rbind("Players", header.Row)
  
  away_advandced_box_score_stats <- box_score_tables[[2]] %>%
    setnames(header.Row) %>%
    filter(!Players %in% c("Starters", "Reserves")) %>%
    select(everything(), -MP)
  
  away_box_score <- away_basic_box_score_stats %>%
    left_join(away_advandced_box_score_stats, by = "Players")
}

game_url <- "https://www.basketball-reference.com/boxscores/201611010NOP.html"

View(away_box_score)
