plotDerozanVsJamesPointStat <- function(games_derozan_vs_james,
                                        derozan_avgs_other_games) {
    
    pointStatList <- getAllPointStats()

    games_derozan_vs_james %>%
        .[, c("Date", "Tm", pointStatList), with = FALSE] %>%
        melt(id.vars = c("Date", "Tm")) %>%
        setnames(c("Date", "Team", "Stat", "Value")) %>%
        ggplot(aes(x = as.factor(Date), y = Value, color = Team)) +
        geom_point(size = 3) +
        scale_color_manual(
            breaks = c("TOR", "SAS"),
            values = c("TOR" = "#CE1141", "SAS" = "#C4CED4")
        ) +
        geom_hline(yintercept = 0, color = "gray82") +
        geom_hline(
            data       = derozan_avgs_other_games,
            mapping    = aes(yintercept = Value),
            color      = "gray60",
            linetype   = "dashed"
        ) +
        labs(
            title    = "DeRozan's stats vs James",
            subtitle = "Dashed line represents Avg. when NOT against James",
            x        = "",
            y        = "stat",
            caption  = "Data: Regular season games\nDeRozan played in, since '14-15"
        ) +
        facet_wrap(~Stat, scales = "free_y", ncol = 3) +
        theme_minimal() +
        theme(
            axis.text.x   = element_text(angle = 45, hjust = 1),
            text          = element_text(size = 12, family = "OCR A Extended"),
            plot.title    = element_text(size = 20),
            legend.position = "top",
            legend.title    = element_blank()
        ) +
        theme(panel.spacing = unit(1.25, "lines"))

    ggsave(
        filename = "DeRozan vs LBJ/Figures/derozan_vs_james.png",
        device = "png", 
        dpi = 600, 
        width = 12, 
        height = 7
    )
}


getAllPointStats <- function() {
    c("Point Difference", "+/-", "PTS", "TRB", "AST", "TOV")
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
        facet_grid(variable~Stat) +
        theme_minimal() +
        theme(
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            axis.text.y      = element_blank(),
            text             = element_text(size = 12, family = "OCR A Extended"),
            plot.title       = element_text(size = 20),
            legend.position  = "top",
            legend.title     = element_blank()) +
        theme(panel.spacing = unit(1.25, "lines"))

    ggsave(
        filename = "DeRozan vs LBJ/Figures/derozan_beta_distributions.png",
        device = "png", 
        dpi = 600, 
        width = 12, 
        height = 7
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

