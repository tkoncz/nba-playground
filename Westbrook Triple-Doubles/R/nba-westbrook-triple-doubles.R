source("Westbrook Triple-Doubles/Global.R")

seasons <- c(2017:2018)

## ----
westbrook_raw_data_from_br <- map_df(seasons, getWestbrookRawStatsFromBR)

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
westbrook_data_from_br %>% 
  .[, `Triple-Double` := ifelse(PTS >= 10 & AST >= 10 & TRB >= 10, TRUE, FALSE)]

westbrook_data_from_br[!is.na(`Triple-Double`), .(Games = .N, Won = sum(`Won?`)), by = `Triple-Double`] %>% 
  .[, `:=`(Lost = Games - Won,
          `Win%` = Won / Games)] %>% 
  .[]

plotWinPctVsRussHadTDorNot()
## TODO: this runs on hardcoded numbers as of now


## ----
