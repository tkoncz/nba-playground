url <- "https://www.basketball-reference.com/boxscores/shot-chart/201803140BOS.html"

extractShootingDataFromGameShotChartUrl <- function(game_url) {
## TODO: needs to be enhanced with "behind half court" shots -- play-by-play
    html <- xml2::read_html(game_url)
    shot_nodes <- rvest::html_nodes(html, xpath = "//div[contains(@id, 'shots-')]//div")
    style <- rvest::html_attr(x = shot_nodes, name = "style")
    tip   <- rvest::html_attr(x = shot_nodes, name = "tip")

    data.table::data.table(
        quarter = as.integer(stringr::str_match(
                tip, 
                "([1-4])(st|nd|rd|th) (quarter|overtime)"
            )[, 2]) + ifelse(stringr::str_match(
                tip, 
                "([1-4])(st|nd|rd|th) (quarter|overtime)")[, 4] == "overtime",
            4, 0),
        time_remaining = stringr::str_match(tip, "[0-9]+:[0-9]+.[0-9]+")[, 1], ##this is a very rough shot at the regex
        player = stringr::str_match(tip, "(<br>)(.+) (made|missed)")[, 3],
        top_px = stringr::str_match(style, "top:([0-9]{1,3})px;")[, 2],
        left_px = stringr::str_match(style, "left:([0-9]{1,3})px;")[, 2],
        distance_ft = as.integer(stringr::str_match(tip, "(from )([0-9]+) ft")[, 3]), 
        points_attempt = as.integer(stringr::str_match(tip, "([2-3])-pointer")[, 2]),
        made_or_missed = stringr::str_extract(tip, "made|missed")
    )
}

shots <- extractShootingDataFromGameShotChartUrl(url)
shots[, .(FG = sum(ifelse(made_or_missed == "made", 1, 0)), FGA = .N), by = player][order(-FGA)]