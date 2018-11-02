source("DeAndre Jordan Free Throws/Global.R")

dj_player_id <- getPlayerIDFromName("DeAndre Jordan")
seasons <- c(2009:2019)

## ----
dj_game_logs <- map(seasons, ~{
    getPlayerGameLogsForSeasonFromBR(
        player_id = dj_player_id,
        season = .x,
        refetch = FALSE,
        folder = "DeAndre Jordan Free Throws/Data"
    )
}) %>% rbindlist()

fixColumnFormats_(dj_game_logs)
addResultColumns_(dj_game_logs)
## TODO: some rows are not removed which should have been... eg Tm == "Tm"

## ----
dj_carreer_ft_pct <- dj_game_logs[season < 2019]  %>% calculateFreeThrowAvg()
dj_2018_ft_pct    <- dj_game_logs[season == 2018] %>% calculateFreeThrowAvg()

## ----
dj_ft_summary_by_season <- summarizeFreeThrowShooting(dj_game_logs)

plotFreeThrowShootingBySeason(
    player_summary_by_season = dj_ft_summary_by_season, 
    folder = "DeAndre Jordan Free Throws/Figures"
)

## ----
seed <- 93
simulation_runs <- 1000000L
dj_2019_FTA <- dj_game_logs[season == 2019, sum(FTA)]
dj_2019_FT  <- dj_game_logs[season == 2019, sum(FT)]

simulated_FT_performance <- simulateFTPerformance(
    seed, simulation_runs, FTA, dj_career_ft_pct, dj_2018_ft_pct
)

plotSimulatedFTPerformance(
    simulated_FT_performance, folder = "DeAndre Jordan Free Throws/Figures",
    dj_2019_FTA, simulation_runs
)

