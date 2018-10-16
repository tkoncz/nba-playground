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
    .[, `Point Difference` := as.numeric(str_match(Result, "\\((\\+|-)(.+)\\)")[, 3])] %>%
    .[, `Point Difference` := ifelse(!`Won?`, -1, 1) * `Point Difference`]
}


fixColumnFormats_ <- function(dt) {
  dt %>% 
    .[, `:=`(G     = as.numeric(G),
             Date  = as.Date(Date, format = "%Y-%m-%d"),
             FG    = ifelse(FG    == "Did Not Play", NA, as.numeric(FG)), 
             FGA   = ifelse(FGA   == "Did Not Play", NA, as.numeric(FGA)), 
             `FG%` = ifelse(`FG%` == "Did Not Play", NA, as.numeric(`FG%`)), 
             `3P`  = ifelse(`3P`  == "Did Not Play", NA, as.numeric(`3P`)), 
             `3PA` = ifelse(`3PA` == "Did Not Play", NA, as.numeric(`3PA`)), 
             `3P%` = ifelse(`3P%` == "Did Not Play", NA, as.numeric(`3P%`)), 
             FT    = ifelse(FT    == "Did Not Play", NA, as.numeric(FT)), 
             FTA   = ifelse(FTA   == "Did Not Play", NA, as.numeric(FTA)), 
             `FT%` = ifelse(`FT%` == "Did Not Play", NA, as.numeric(`FT%`)), 
             ORB   = ifelse(ORB   == "Did Not Play", NA, as.numeric(ORB)),  
             DRB   = ifelse(DRB   == "Did Not Play", NA, as.numeric(DRB)), 
             TRB   = ifelse(TRB   == "Did Not Play", NA, as.numeric(TRB)), 
             AST   = ifelse(AST   == "Did Not Play", NA, as.numeric(AST)), 
             STL   = ifelse(STL   == "Did Not Play", NA, as.numeric(STL)), 
             BLK   = ifelse(BLK   == "Did Not Play", NA, as.numeric(BLK)), 
             TOV   = ifelse(TOV   == "Did Not Play", NA, as.numeric(TOV)), 
             PF    = ifelse(PF    == "Did Not Play", NA, as.numeric(PF)), 
             PTS   = ifelse(PTS   == "Did Not Play", NA, as.numeric(PTS)), 
             GmSc  = ifelse(GmSc  == "Did Not Play", NA, as.numeric(GmSc)))]
}


plotInGameStatsVsPointDiff <- function(dt) {

  in_game_statistics <- getListOfInGameStatistics()
  
  dt %>% 
    .[, mget(c("Point Difference", "Won?", in_game_statistics))] %>% 
    melt(id.vars = c("Point Difference", "Won?"), measure.vars = in_game_statistics) %>% 
    ggplot(aes(x = value, y = `Point Difference`)) +
    geom_point(aes(color = `Won?`)) +
    facet_wrap(~variable, scales = "free_x", ncol = 6) +
    geom_smooth(method = "lm") +
    labs(title = "Russ' in game stats vs. final point difference",
         subtitle = "Based on the '16-17 and '17-18 regular seasons",
         x     = "") +
    scale_color_manual(values=c("gray82", "#0072CE")) +
    theme_minimal() +
    # theme(panel.grid.minor = element_blank(),
    #       panel.grid.major = element_blank()) +
    theme(text          = element_text(size = 11, family = "OCR A Extended")) +
    theme(plot.title    = element_text(size = 20),
          plot.caption  = element_text(color = "gray60")) +
    theme(legend.position = "none")
  
  ggsave(
    filename = "Westbrook Triple-Doubles/Figures/westbrook_in_game_stats_vs_point_difference.png",
    device = "png", 
    dpi = 600, 
    width = 12, 
    height = 8
  )
}


getListOfInGameStatistics <- function() {
  c(
    "FG",  "3P",  "FT",  "TRB", "AST", "PTS",
    "FGA", "3PA", "FTA", "ORB", "TOV", "PF",
    "FG%", "3P%", "FT%", "DRB", "STL", "BLK"
  )
}