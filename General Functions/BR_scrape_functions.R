getPlayerIDFromName <- function(name, return_data_table = FALSE) {
    all_player_info <- getAllPlayerIDsAndPageURLS(
        refetch = FALSE,
        folder = "General Data" 
    )

    player_info_with_name <- all_player_info[grep(
        pattern     = name,
        x           = player_name,
        ignore.case = TRUE,
        perl        = TRUE
    )]
    has_multiple_matches  <- player_info_with_name[, .N] > 1
    has_no_match          <- player_info_with_name[, .N] < 1

    if(return_data_table == TRUE) {
        if(has_multiple_matches == TRUE) {
            warning(
                glue("Multiple players returned for criteria name ~ '{name}' ..."),
                call. = FALSE
            )
        } else if(has_no_match == TRUE) {
            issueWarningNoPlayerMatched(name)
        }
        return(player_info_with_name)
    } else {
        player_ids <- player_info_with_name[["player_id"]]

        if(has_multiple_matches == TRUE) {
            warning(
                glue(
                    "Multiple players returned for criteria name ~ '{name}' ...\n",
                    "Use return_data_table = TRUE to see complete player infos"
                ),
                call. = FALSE
            )
        } else if(has_no_match == TRUE) {
            issueWarningNoPlayerMatched(name)
        }
        return(player_ids)
    }
}


issueWarningNoPlayerMatched <- function(name) {
    warning(
        glue("No players matched for criteria name ~ '{name}' ..."),
        call. = FALSE
    )    
}


getPlayerGameLogsForSeasonFromBR <- function(player_id,
                                             season,
                                             refetch = FALSE,
                                             folder) {
    
    file_path <- glue("{folder}/raw_{season}_game_logs_for_{player_id}.csv")

    if(refetch == TRUE | !file.exists(file_path)) {
        message("Querying from basketball-reference.com...")

        raw_player_game_log_for_season <- glue(
            "https://www.basketball-reference.com/players/w/{player_id}/gamelog/{season}"
        ) %>% 
            getTableFromHTML(8) %>%
            fixColumnNamesInPlayerGameLog_()

        raw_player_advanced_game_log_for_season <- glue(
            "https://www.basketball-reference.com/players/w/{player_id}/gamelog-advanced/{season}"
        ) %>%
            getTableFromHTML(1) %>%
            selectOnlyAdvancedStats()            
            
        raw_player_game_log_for_season_w_advanced <- merge(
            raw_player_game_log_for_season,
            raw_player_advanced_game_log_for_season,
            by = "Rk",
            all.x = TRUE
        ) %>% 
            .[, season := season] %>% 
            .[, G := Rk] %>%
            .[, Rk := NULL]

        fwrite(x = raw_player_game_log_for_season_w_advanced, file = file_path)

    } else {
        message("Loading from existing .csv file...")

        raw_player_game_log_for_season_w_advanced <- fread(file_path)
    }

    raw_player_game_log_for_season_w_advanced
}


getTableFromHTML <- function(url, table_num) {
    url %>% 
    read_html() %>%
        html_nodes("table") %>% 
        `[[`(table_num) %>% 
        html_table(header = TRUE, fill = TRUE) %>% 
        as.data.table()
}


fixColumnNamesInPlayerGameLog_ <- function(dt) {
    dt %>% 
        setnames(c("Rk", "G",  "Date",  "Age", "Tm", "Away",
                   "Opp", "Result", "GS", "MP", "FG", "FGA",
                   "FG%", "3P", "3PA", "3P%", "FT", "FTA",
                   "FT%", "ORB", "DRB", "TRB", "AST", "STL",
                   "BLK", "TOV", "PF", "PTS", "GmSc", "+/-"))
}


selectOnlyAdvancedStats <- function(advanced_game_log) {
    advanced_game_log[, 
        .(Rk, `TS%`, `eFG%`, `ORB%`, `DRB%`, `TRB%`, `AST%`, 
          `STL%`, `BLK%`, `TOV%`, `USG%`, ORtg, DRtg)
    ]
}


getAllPlayerIDsAndPageURLS <- function(refetch = FALSE, folder) {
    file_path <- glue("{folder}/all_player_info.csv")

    if(refetch == TRUE | !file.exists(file_path)) {
        message("Querying from basketball-reference.com...")
        
        all_player_info <- map_df(letters, ~{
            getPageURLsForPlayersWithLastNameStartingWithLetter(.x)
        }) %>%
            .[, 
                player_id := str_match(
                    string = player_page_link, 
                    pattern = "/players/[a-z]/([a-z]+[0-9]+).html"
                ) %>% .[, 2]
            ]

        fwrite(x = all_player_info, file = file_path)

    } else {
        message("Loading from existing .csv file...")

        all_player_info <- fread(file_path)
    }

    all_player_info
}

getPageURLsForPlayersWithLastNameStartingWithLetter <- function(last_name_start_letter) {

    last_name_start_letter <- tolower(last_name_start_letter)

    message(glue("Getting player pages for letter: {last_name_start_letter}..."))

    player_list_url <- glue(
        "https://www.basketball-reference.com/players/{last_name_start_letter}/"
    )
    xpath_condition <- (
        "//div[@id= 'all_players']//*[starts-with(@href, '/players/') and contains(@href, '.html')]"
    )

    html_nodes_with_player_links <- getHTMLNodesForUrlBasedOnXPath(
        url = player_list_url,
        xpath_condition = xpath_condition
    )

    if(!all(is.na(html_nodes_with_player_links))) {
        player_page_links <-  html_nodes_with_player_links %>% html_attr("href")
        player_names      <-  html_nodes_with_player_links %>% html_text()

        player_pages <- data.table(
            "player_name"      = player_names,
            "player_page_link" = player_page_links,
            "player_last_name_starts_with" = last_name_start_letter
        )
    } else {
        player_pages <- data.table()
    }

    closeAllConnections()
    player_pages
}


getHTMLNodesForUrlBasedOnXPath <- function(url, xpath_condition) {
    tryCatch(
        {
            read_html(url) %>% html_nodes(xpath = xpath_condition)
        }, error = function(cond) {
            message(glue("URL does not seem to exist: {url}"))
            message("Below is the original error message:")
            message(cond)
            message("\n")

            return(NA)
        }
    )
}


fixRawRows <- function(dt) {
  dt[G != "Rk"]
}


addResultColumns_ <- function(dt) {
  dt %>%
    .[, `Won?` := grepl("^W", Result)] %>% 
    .[, `Point Difference` := as.integer(str_match(Result, "\\((\\+|-)(.+)\\)")[, 3])] %>%
    .[, `Point Difference` := ifelse(!`Won?`, -1, 1) * `Point Difference`]
}


fixColumnFormats_ <- function(dt) {
  ## special columns: G, Date
  dt[, `:=`(
    G    = as.integer(G),
    Date = as.Date(Date, format = "%Y-%m-%d"))
    ]
  
  ## special column: Age
  dt[, c("age_years", "age_days") := tstrsplit(Age, "-")] %>%
    .[, Age := as.numeric(age_years) + as.numeric(age_days) / 365] %>%
    .[, `:=`(age_years = NULL, age_days = NULL)] # clean-up
  
  ## special column: MP
  dt[, c("MP_minutes", "MP_seconds") := tstrsplit(MP, ":")] %>%
    .[, MP := as.numeric(MP_minutes) + as.numeric(MP_seconds) / 60] %>%
    .[, MP := ifelse(is.na(MP), 0, MP)] %>%
    .[, `:=`(MP_minutes = NULL, MP_seconds = NULL)] # clean-up
  
  ## numeric columns
  numeric_columns <- list(
    "FG%", "3P%", "FT%", "GmSc",
    "TS%", "eFG%", "ORB%", "DRB%", "TRB%", "AST%", 
    "STL%", "BLK%", "TOV%", "USG%"
  )
  
  walk(numeric_columns, ~{dt[, (.x) := as.numeric(get(.x))]})
  
  ## integer columns
  integer_columns <- list(
    "FG", "FGA", "FT", "FTA", "ORB", "DRB", "TRB", 
    "AST", "STL", "BLK", "TOV", "PF", "PTS", 
    "+/-", "3P", "3PA",
    "ORtg", "DRtg"
  )
  
  walk(integer_columns, ~{dt[, (.x) := as.integer(get(.x))]})
  
  dt
}


getPlayerGameLogsForPlayoffsFromBR <- function(player_id,
                                               season,
                                               refetch = FALSE,
                                               folder) {
    
    file_path <- glue(
        "{folder}/raw_{season}_playoffs_game_logs_for_{player_id}.csv"
    )

    if(refetch == TRUE | !file.exists(file_path)) {
        message("Querying from basketball-reference.com...")

        raw_player_game_log_for_playoffs <- glue(
            "https://www.basketball-reference.com/players/w/{player_id}/gamelog/{season}"
        ) %>%
            getPlayerPlayoffGameLogTableFromHTML() %>%
            fixColumnNamesInPlayerGameLog_()

        raw_player_advanced_game_log_for_playoffs <- glue(
            "https://www.basketball-reference.com/players/w/{player_id}/gamelog-advanced/{season}"
        ) %>%
            getPlayerPlayoffGameLogTableFromHTML() %>%
            selectOnlyAdvancedStats()            
            
        raw_player_game_log_for_playoffs_w_advanced <- merge(
            raw_player_game_log_for_playoffs,
            raw_player_advanced_game_log_for_playoffs,
            by = "Rk",
            all.x = TRUE
        ) %>% 
            .[, season := season] %>% 
            .[, G := Rk] %>%
            .[, Rk := NULL]

        fwrite(x = raw_player_game_log_for_playoffs_w_advanced, file = file_path)

    } else {
        message("Loading from existing .csv file...")

        raw_player_game_log_for_playoffs_w_advanced <- fread(file_path)
    }

    raw_player_game_log_for_playoffs_w_advanced
}


getPlayerPlayoffGameLogTableFromHTML <- function(url) {
    url %>%
        read_html() %>% 
        html_nodes(xpath = '//comment()') %>%
        html_text() %>%
        paste0(collapse = "")  %>%
        read_html() %>% 
        html_nodes(xpath = "//table") %>% 
        html_table() %>% 
        as.data.table()
}