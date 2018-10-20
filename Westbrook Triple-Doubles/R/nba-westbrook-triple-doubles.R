source('Westbrook Triple-Doubles/Global.R')

seasons <- c(2017:2018)

## ----
westbrook_raw_data_from_br <- map(seasons, ~{getRawPlayerGameLogsForSeasonFromBR("westbru01", .x)}) %>% rbindlist()

fixRawColumns_(westbrook_raw_data_from_br)
westbrook_data_from_br <- fixRawRows(westbrook_raw_data_from_br)

addResultColumns_(westbrook_data_from_br)

## ----
fixColumnFormats_(westbrook_data_from_br)
## TODO: fix Age, MP column formats

## ----
plotInGameStatsVsPointDiff(westbrook_data_from_br)
## TODO: add subtitle to show which seasons are shown


## ----
westbrook_data_from_br[, 
  `Triple-Double` := ifelse(PTS >= 10 & AST >= 10 & TRB >= 10, TRUE, FALSE)
]
## TODO: handle other types of TD

russ_won_lost_by_had_td <- createBreakdownOfWinLossByTDorNot(westbrook_data_from_br)
russ_won_lost_by_had_td %>% .[]

plotWinPctVsPlayerHadTDorNot(russ_won_lost_by_had_td)


## ----
plotPointDiffVsStatsRegressionCoeffs(westbrook_data_from_br)


## ----
plotASTandTRBDistributions(westbrook_data_from_br)
