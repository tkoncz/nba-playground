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

#returns a df for a html box score table
fun_get_box_score_table <- function(df) {
  header.Row <- df[1, -1] %>%
    t()
  rownames(header.Row) <- c()
  header.Row <- rbind("Players", header.Row)
  
  box_score_stats <- df %>%
    setnames(header.Row) %>%
    filter(!Players %in% c("Starters", "Reserves"))
  
  return(box_score_stats)
}

fun_get_game_box_score <- function(url) {
  html <- read_html(game_url)
  node <- html_nodes(html, "table")
  box_score_tables <- html_table(node, header = T)

  #basic
  away_basic_box_score_stats <- fun_get_box_score_table(box_score_tables[[1]])
  home_basic_box_score_stats <- fun_get_box_score_table(box_score_tables[[3]])
  
  #advanced
  away_advanced_box_score_stats <- fun_get_box_score_table(box_score_tables[[2]])
  home_advanced_box_score_stats <- fun_get_box_score_table(box_score_tables[[4]])
  
  away_box_score <- away_basic_box_score_stats %>%
    left_join(away_advanced_box_score_stats, by = "Players") %>%
    mutate(Starter.Reserve = ifelse(row_number() <= 5, "Starter", "Reserve")) %>%
    mutate(Home.Away = "Away")
  
  home_box_score <- home_basic_box_score_stats %>%
    left_join(home_advanced_box_score_stats, by = "Players") %>%
    mutate(Starter.Reserve = ifelse(row_number() <= 5, "Starter", "Reserve")) %>%
    mutate(Home.Away = "Home")
  
  box_score <- bind_rows(away_box_score, home_box_score) %>%
    mutate(Game.Url = url)
  
  return(box_score)
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

box_scores_list <- vector("list", 1200)
i=1
for (game_url in dt_Schedule$Game.URL[1:1200]) {
  box_scores_list[[i]] <- fun_get_game_box_score(game_url)
  i=i+1
}

box_scores_list[[1]]

https://www.basketball-reference.com/boxscores/200011020PHO.html
https://www.basketball-reference.com/boxscores/200011020PHO.html