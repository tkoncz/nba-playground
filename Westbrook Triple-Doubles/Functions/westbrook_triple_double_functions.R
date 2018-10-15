getWestbrookRawStatsFromBR <- function(season) {
  
  season_url <- glue("https://www.basketball-reference.com/players/w/westbru01/gamelog/{season}")
  
  read_html(season_url) %>%
    html_nodes("table") %>% 
    `[[`(8) %>% 
    html_table(header = TRUE, fill = TRUE) %>% 
    as.data.table() %>%
    .[, season := season]
}


fixRawColumns_ <- function(westbrook_raw_data_from_br) {
  westbrook_raw_data_from_br %>% 
    setnames(c("Rk", "G",  "Date",  "Age", "Tm", "Away",
               "Opp", "Result", "GS", "MP", "FG", "FGA",
               "FG%", "3P", "3PA", "3P%", "FT", "FTA",
               "FT%", "ORB", "DRB", "TRB", "AST", "STL",
               "BLK", "TOV", "PF", "PTS", "GmSc", "+/-",
               "season", "V1", "V2", "V3")) %>% 
    .[season == 2018, `:=`(Away = V1,
                           Result = V2)] %>% 
    .[, `:=`(V1 = NULL,
             V2 = NULL,
             V3 = NULL)] %>% 
    .[, G := Rk] %>%
    .[, Rk := NULL]
}


fixRawRows <- function(dt) {
  dt[G != "Rk"]
}

addResultColumns_ <- function(dt) {
  dt %>%
    .[, `Won?` := grepl("^W", Result)] %>% 
    .[, `Score Difference` := as.numeric(str_match(Result, "\\((\\+|-)(.+)\\)")[, 3])] %>%
    .[, `Score Difference` := ifelse(!`Won?`, -1, 1) * `Score Difference`]
}