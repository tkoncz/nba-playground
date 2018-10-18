source("Westbrook Triple-Doubles/Global.R")

seasons <- c(2017:2018)

## ----
westbrook_raw_data_from_br <- map(seasons, ~{getRawStatsFromBR("westbru01", .x)}) %>% rbindlist()

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
## TODO: this runs on hardcoded numbers as of now

## ----
# in_game_statistics <- getListOfInGameStatistics()
# 
# data_for_glm <- westbrook_data_from_br[, mget(c("Point Difference", in_game_statistics))] %>% 
#   .[, `:=`(`FG%` = NULL,
#            `3P%` = NULL,
#            `FT%` = NULL,
#            PTS   = NULL,
#            TRB   = NULL)] %>% 
#   .[!is.na(FGA)]
# 
# lm_fit <- lm(
#   formula = formula("`Point Difference` ~ ."),
#   data = data_for_glm
# )
# 
# summary(lm_fit)$r.squared
# lm_fit$coefficients %>% 
#   as.data.table(keep.rownames = T) %>% 
#   setnames(c("Stat", "Value")) %>%
#   .[, Stat := gsub("`", "", Stat)] %>% 
#   .[, Stat := forcats::fct_reorder(Stat, Value)] %>% 
#   .[Stat != "(Intercept)"] %>% 
#   ggplot(aes(x = Stat, y = Value)) +
#   geom_bar(stat = "identity", aes(fill = ifelse(Value > 0, "pos", "neg"))) +
#   scale_fill_manual(values = c("neg" = "#EF3B24", "pos" = "#007AC1")) +
#   coord_flip() +
#   labs(title = "Point Diff. impact of an extra Russ...",
#        subtitle = "Based on the '16-17 and '17-18 regular seasons",
#        x = "",
#        y = "") +
#   theme_minimal() +
#   # theme(panel.grid.minor = element_blank(),
#   #       panel.grid.major = element_blank()) +
#   theme(text          = element_text(size = 11, family = "OCR A Extended")) +
#   theme(plot.title    = element_text(size = 18),
#         plot.caption  = element_text(color = "gray60")) +
#   theme(legend.position = "none")
# 
# ggsave(
#   filename = "Westbrook Triple-Doubles/Figures/westbrook_in_game_stats_coefficients.png",
#   device = "png", 
#   dpi = 600, 
#   width = 10, 
#   height = 6
# )


## ----
#frequency of ~9 AST/ RBD games