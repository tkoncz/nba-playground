calculateFreeThrowAvg <- function(player_game_logs) {
    player_game_logs[, sum(FT, na.rm = TRUE) / sum(FTA, na.rm = TRUE)]
}

summarizeFreeThrowShooting <- function(player_game_logs, folder) {
    player_game_logs %>%
        .[, 
            .(FT = sum(FT, na.rm = TRUE), FTA = sum(FTA, na.rm = TRUE)), 
            keyby = season
        ] %>%
        .[, `FT%` := FT / FTA] %>%
        .[, `:=`(
            `FT% - lower` = qbeta(0.025, FT, FTA - FT),
            `FT% - upper` = qbeta(0.975, FT, FTA - FT)
        )] 
}


plotFreeThrowShootingBySeason <- function(player_summary_by_season, folder) {   
    player_summary_by_season %>%
        .[, Team := ifelse(season < 2019, "LAC", "DAL")] %>%
        ggplot(aes(x = as.factor(season), y = `FT%` * 100, color = Team, group = 1)) +
            geom_point(aes(size = FTA)) +
            geom_linerange(aes(ymin = `FT% - lower` * 100, ymax = `FT% - upper` * 100)) +
            geom_line() +
            scale_y_continuous(limits = c(0, 100)) +
            scale_color_manual(values = c("LAC" = "#C8102E", "DAL" = "#00538C")) +
            labs(
                title = "DeAndre Jordan's FT shooting over the years",
                x = "Season",
                y = "Free Throw %",
                caption = "Based on regular season data since '08-09"
            ) +
            theme_minimal() +
            theme(
                axis.text.x   = element_text(angle = 45, hjust = 1),
                text          = element_text(size = 12, family = "OCR A Extended"),
                plot.title    = element_text(size = 18),
                legend.position = "none"
            )

    ggsave(
        filename = glue("{folder}/deandre_ft_pct_over_seasons.png"),
        dpi = 300, width = 8, height = 6
    )
}


simulateFTPerformance <- function(seed, simulation_runs,
                                  FTA, dj_career_ft_pct, dj_2018_ft_pct) {
    set.seed(seed)
    simulation_results_career_ft <- data.table(
            FT = rbinom(simulation_runs, dj_2019_FTA, dj_carreer_ft_pct)
    ) %>% .[, .(Career = .N / simulation_runs), by = FT]

    set.seed(seed)
    simulation_results_2018_ft <- data.table(
        FT = rbinom(simulation_runs, dj_2019_FTA, dj_2018_ft_pct)
    ) %>% .[, .(`2018` = .N / simulation_runs), by = FT]
        
    merge(
        simulation_results_career_ft, simulation_results_2018_ft,
        by = "FT", all = TRUE
    ) %>% .[]
}


plotSimulatedFTPerformance <- function(simulated_FT_performance, folder,
                                       dj_2019_FTA, simulation_runs) {
    ggplot(simulated_FT_performance, aes(x = as.integer(FT))) +
        geom_rect(
            aes(
                xmin = dj_2019_FT - .5, xmax = dj_2019_FTA + .5, 
                ymin  = 0, ymax = .15
            ), 
            fill = "#B8C4CA"
        ) +
        geom_text(
            aes(x = dj_2019_FT - .5, y = .15), hjust = 0, vjust = 1, 
            label = "Results at least\nmatching DeAndre's\nshooting in 2019",
            family = "OCR A Extended", color = "#002B5E", size = 3
        ) +
        geom_bar(aes(y = Career), stat = "identity", alpha = 0.75, fill = "#C8102E") +
        geom_text(
            aes(x = 10, y = .0825), label = "Career FT%", 
            family = "OCR A Extended", color = "#C8102E"
        ) +
        geom_bar(aes(y = `2018`), stat = "identity", alpha = 0.75, fill = "#1D42BA") +
        geom_text(
            aes(x = 25, y = .0825), label = "'17-18 FT%", 
            family = "OCR A Extended", color = "#1D42BA"
        ) +
        scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30)) +
        labs(
            title = "Simulating DeAndre's 2019 Free Throw Shooting",
            x = glue("Free Throws Made on {dj_2019_FTA} attempts"),
            y = "Percentage of all outcomes",
            caption = glue("Data based on\n{simulation_runs} simulation runs") 
        ) +
        theme_minimal() +
        theme(
            panel.grid.minor = element_blank(),
            text          = element_text(size = 12, family = "OCR A Extended"),
            plot.title    = element_text(size = 18),
            legend.position = "none"
        )

    ggsave(
        filename = glue("{folder}/deandre_ft_simulation.png"),
        dpi = 300, width = 8, height = 6
    )
}