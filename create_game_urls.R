library(data.table)

#from original date format to R date
date_2_dateKey <- function(date_String) {
  date <- substr(date_String, 6, nchar(date_String))
  date <- as.Date(date, format = "%B %d, %Y")
  return(date)
}

#creates game url based on dt columns
create_game_url <- function(Date_Key, TeamCode) {
  base_url <- "https://www.basketball-reference.com/boxscores/"
  url <- paste(base_url, Date_Key, "0", TeamCode, ".html", sep= "")
  return(url)
}

dt <- fread("schedulesTable.csv")
dt_teamNameMapping <- fread("teamNameMapping.csv")

dt[, c("V7", "V8", "Notes") := NULL]
dt <- dt[`Start (ET)` != "Playoffs"]

setnames(x = dt, 
         old = 1:6, 
         new = c("Date", "Start_ET", "Away.Team", "Away.Points", "Home.Team", "Home.Points") )

setnames(x = dt_teamNameMapping,
         old = "Team.Name",
         new = "Home.Team")

dt <- merge(x=dt, y=dt_teamNameMapping, all.x=TRUE)
#dt[, .N, keyby = .(Away.Team, Team.Code)]

dt[, Date := date_2_dateKey(Date)]
dt[, Date_Key := format(Date, "%Y%m%d")]

dt[, Game.URL := create_game_url(as.character(Date_Key), Team.Code)]


html <- xml2::read_html("https://www.basketball-reference.com/boxscores/201611010NOP.html")
node <- rvest::html_nodes(html, "table")
table <- rvest::html_table(node, header = TRUE)
data.table(table[[1]])
