source('DeRozan vs LBJ/Global.R')

seasons <- c(2015:2019)
james_player_id <- getPlayerIDFromName("Lebron James", FALSE)
derozan_player_id <- getPlayerIDFromName("Demar Derozan", FALSE)

## ---- download data
james_data_from_br <- map(seasons, ~{
    getPlayerGameLogsForSeasonFromBR(
        player_id   = james_player_id, 
        season      = .x, 
        refetch     = FALSE,
        folder      = "DeRozan vs LBJ/Data")
}) %>% rbindlist()

derozan_data_from_br <- map(seasons, ~{
    getPlayerGameLogsForSeasonFromBR(
        player_id   = derozan_player_id, 
        season      = .x, 
        refetch     = FALSE,
        folder      = "DeRozan vs LBJ/Data")
}) %>% rbindlist()

## ---- fix formatting issues
fixColumnFormats_(james_data_from_br)
fixColumnFormats_(derozan_data_from_br)

## ----
addResultColumns_(derozan_data_from_br)
    
## ---- 
game_dates_james_vs_tor <- c(
    james_data_from_br[season < 2019  & Opp == "TOR" & MP > 0, Date],
    james_data_from_br[season >= 2019 & Opp == "SAS" & MP > 0, Date]
)

games_derozan_vs_james <- derozan_data_from_br[
    Date %in% game_dates_james_vs_tor & MP > 0 
]

games_derozan_not_against_james <- derozan_data_from_br[
    !(Date %in% game_dates_james_vs_tor) & MP > 0
] 

derozan_avgs_other_games <- games_derozan_not_against_james %>%
    .[season < 2019] %>%
    .[, .(
        `Point Difference` = mean(`Point Difference`),
        `+/-`              = mean(`+/-`),
         PTS               = mean(PTS),
         AST               = mean(AST),
         TOV               = mean(TOV),
         TRB               = mean(TRB)
        )
    ] %>%
    melt() %>%
    setnames(c("Stat", "Value"))

## ----
plotDerozanVsJamesPointStat(games_derozan_vs_james, derozan_avgs_other_games)
        
## ----
#FG, 3P, FT, TRB, AST, TOV
derozan_summary <- rbind(
    games_derozan_not_against_james %>% 
        copy() %>% 
        .[, group := ifelse(Tm == "TOR", "Other - TOR", "Other - SAS")],
    games_derozan_vs_james %>% 
        copy() %>% 
        .[, group := ifelse(Tm == "TOR", "Vs. James - TOR", "Vs. James - SAS")]
) %>% 
    .[, 
      .(
        FG_alpha   = sum(FG),    FG_beta  = sum(FGA) - sum(FG),
        `3P_alpha` = sum(`3P`), `3P_beta` = sum(`3PA`) - sum(`3P`),
        FT_alpha   = sum(FT),    FT_beta  = sum(FTA) - sum(FT)
      ), 
      by = group]

plotBetaDistributionsForDerozanStats()
## TODO: factor order...
