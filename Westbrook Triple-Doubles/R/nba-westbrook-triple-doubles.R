source("Westbrook Triple-Doubles/Global.R")

seasons <- c(2017:2018)

westbrook_raw_data_from_br <- map_df(seasons, getWestbrookRawStatsFromBR)

fixRawColumns_(westbrook_raw_data_from_br)
westbrook_data_from_br <- fixRawRows(westbrook_raw_data_from_br)

addResultColumns_(westbrook_data_from_br)