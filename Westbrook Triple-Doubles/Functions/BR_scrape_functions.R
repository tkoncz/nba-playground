player_ids <- getAllPlayerIDsAndPageURLS(abc = c("c", "w"))

getAllPlayerIDsAndPageURLS <- function(abc = letters) {
    player_pages <- map_df(abc, ~{
        getPageURLsForPlayersWithLastNameStartingWithLetter(.x)
    })

    player_pages[, 
        player_id := str_match(
            string = player_page_link, 
            pattern = "/players/[a-z]/([a-z]+[0-9]+).html"
        ) %>% .[, 2]
    ]

    player_pages
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
            "player_page_link" = player_page_links
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