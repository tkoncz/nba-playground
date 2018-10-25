source('Westbrook Triple-Doubles/Global.R')

seasons <- c(2017:2018)
player_name <- "Russell Westbrook"
player_id <- getPlayerIDFromName(player_name, FALSE)

## ---- download data
westbrook_data_from_br <- map(seasons, ~{
    getPlayerGameLogsForSeasonFromBR(
        player_id   = player_id, 
        season      = .x, 
        refetch     = FALSE,
        folder      = "Westbrook Triple-Doubles/Data")
}) %>% rbindlist()

## -- fix general formatting issues
addResultColumns_(westbrook_data_from_br)
fixColumnFormats_(westbrook_data_from_br)

## ---- stand alone relationship of game's final point diff. vs. player stats
plotInGameStatsVsPointDiff(westbrook_data_from_br)

## ---- Win% distr. for triple-double and non-triple-double games
westbrook_data_from_br <- flagDoubleTripleQuadrupleDoublesForPlayer_(westbrook_data_from_br)

russ_won_lost_by_had_td <- createBreakdownOfWinLossByTDorNot(westbrook_data_from_br)
russ_won_lost_by_had_td %>% .[]

plotWinPctVsPlayerHadTDorNot(russ_won_lost_by_had_td)

## ---- Simple shot at separating the impact of different factors
plotPointDiffVsStatsRegressionCoeffs(westbrook_data_from_br)

## ---- How about stat padding?
plotASTandTRBDistributions(westbrook_data_from_br)