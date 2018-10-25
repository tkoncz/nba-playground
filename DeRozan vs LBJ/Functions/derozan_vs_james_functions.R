plotAllDerozanVsJamesPointStats <- function(games_derozan_vs_james,
                                            derozan_avgs_other_games,
                                            pointPlotTable) {
    walk(1:pointPlotTable[, .N], ~{
        plotDerozanVsJamesPointStat (
            games_derozan_vs_james = games_derozan_vs_james,
            derozan_avgs_other_games = derozan_avgs_other_games,
            stat = pointPlotTable[.x, stat],
            plot_title = pointPlotTable[.x, plot_title],
            plot_save_path = pointPlotTable[.x, plot_save_path]
        )
    })
}


getAllPointStats <- function() {
    figure_folder_path <- "DeRozan vs LBJ/Figures"
    data.table(
        stat = c(
            "Point Difference", 
            "+/-",
            "PTS",
            "AST",
            "TOV",
            "TRB"
        ),
        plot_title = c(
            "DeRozan's vs James' team results", 
            "DeRozan's plus/minus vs James' teams",
            "DeRozan's Points vs James' teams",
            "DeRozan's Assists vs James' teams",
            "DeRozan's Turnovers vs James' teams",
            "DeRozan's Rebounds vs James' teams"
        ),
        plot_save_path = c(
            glue("{figure_folder_path}/derozan_vs_james_point_difference.png"),
            glue("{figure_folder_path}/derozan_vs_james_plus_minus.png"),
            glue("{figure_folder_path}/derozan_vs_james_points.png"),
            glue("{figure_folder_path}/derozan_vs_james_assists.png"),
            glue("{figure_folder_path}/derozan_vs_james_turnovers.png"),
            glue("{figure_folder_path}/derozan_vs_james_rebounds.png")
        )
    )
}


plotDerozanVsJamesPointStat <- function(games_derozan_vs_james,
                                         derozan_avgs_other_games,
                                         stat,
                                         plot_title,
                                         plot_save_path) {
    games_derozan_vs_james %>%
        .[, .(Date, Tm, get(stat))] %>%
        setnames(c("Date", "Team", "Stat")) %>%
        ggplot(aes(x = as.factor(Date), y = Stat, color = Team)) +
        geom_point(size = 3) +
        scale_color_manual(
            breaks = c("TOR", "SAS"),
            values = c("TOR" = "#CE1141", "SAS" = "#C4CED4")
        ) +
        geom_hline(yintercept = 0, color = "gray82") +
        geom_hline(
            yintercept = derozan_avgs_other_games[[stat]],
            color      = "#CE1141",
            linetype   = "dashed"
        ) +
        labs(
            title    = plot_title,
            subtitle = glue("Dashed line represents Avg. {stat} when NOT against James"),
            x        = "",
            y        = stat,
            caption  = "Data: Regular season games\n  DeRozan played in, since '14-15"
        ) +
        theme_minimal() +
        theme(
            axis.text.x   = element_text(angle = 45, hjust = 1),
            text          = element_text(size = 12, family = "OCR A Extended"),
            plot.title    = element_text(size = 20),
            legend.position = "top",
            legend.title    = element_blank()
        )

    ggsave(
        filename = plot_save_path,
        device = "png", 
        dpi = 600, 
        width = 7, 
        height = 5
    )
}


plotBetaDistributionsForDerozanStats <- function() {
    rbind(
        createBetaDistributionForStat(derozan_summary, "FG"),
        createBetaDistributionForStat(derozan_summary, "3P"),
        createBetaDistributionForStat(derozan_summary, "FT")
    ) %>% 
        melt(id.vars = c("pct", "Stat", "Matchup")) %>% 
        ggplot(aes(x = pct, y = value, fill = Matchup)) +
        geom_area(size = 0, position = "dodge", alpha = .75) +
        scale_y_sqrt() +
        scale_fill_manual(
            values = c(
                "Vs. James" = "brown", 
                "Other" = "gray60"
            )) +
        labs(
            title = "DeRozan's Avgs Represented by the Beta Distribution",
            subtitle = "Note: Y axis is on a sqrt scale",
            y = "",
            x = "",
            caption  = "Data: Regular season games\n DeRozan played in, since '14-15") +
        facet_grid(variable~Stat, scales = "free_y") +
        theme_minimal() +
        theme(panel.grid.minor = element_blank(),,
              axis.text.y = element_blank()) +
        theme(text          = element_text(size = 10, family = "OCR A Extended")) +
        theme(plot.title    = element_text(size = 18)) +
        theme(legend.position = "top",
              legend.title=element_blank())

    ggsave(
        filename = "DeRozan vs LBJ/Figures/derozan_beta_distributions.png",
        device = "png", 
        dpi = 600, 
        width = 7, 
        height = 5
    )
}


createBetaDistributionForStat <- function(derozan_summary, stat) {
    vs_james_tor_alpha <- derozan_summary[group == "Vs. James - TOR"] %>% 
        `[[`(glue("{stat}_alpha")) 
    vs_james_tor_beta  <- derozan_summary[group == "Vs. James - TOR"] %>% 
        `[[`(glue("{stat}_beta")) 
    vs_james_sas_alpha <- derozan_summary[group == "Vs. James - SAS"] %>% 
        `[[`(glue("{stat}_alpha")) 
    vs_james_sas_beta  <- derozan_summary[group == "Vs. James - SAS"] %>% 
        `[[`(glue("{stat}_beta")) 
    other_tor_alpha    <- derozan_summary[group == "Other - TOR"] %>% 
        `[[`(glue("{stat}_alpha"))
    other_tor_beta     <- derozan_summary[group == "Other - TOR"] %>% 
        `[[`(glue("{stat}_beta"))
    other_sas_alpha    <- derozan_summary[group == "Other - SAS"] %>% 
        `[[`(glue("{stat}_alpha"))
    other_sas_beta     <- derozan_summary[group == "Other - SAS"] %>% 
        `[[`(glue("{stat}_beta"))
    
    rbind(
        data.table(
            pct      = c(1:1000) / 1000,
            Stat     = glue("{stat}%"),
            Matchup  = "Vs. James",
            `in TOR` = dbeta(c(1:1000) / 1000, vs_james_tor_alpha, vs_james_tor_beta),
            `in SAS` = dbeta(c(1:1000) / 1000, vs_james_sas_alpha, vs_james_sas_beta)
        ),
        data.table(
            pct      = c(1:1000) / 1000,
            Stat     = glue("{stat}%"),
            Matchup  = "Other",
            `in TOR` = dbeta(c(1:1000) / 1000, other_tor_alpha, other_tor_beta),
            `in SAS` = dbeta(c(1:1000) / 1000, other_sas_alpha, other_sas_beta)
        )
    )
} 

