library(data.table)

#from original date format to R date
date_2_dateKey <- function(date_String) {
  date <- substr(date_String, 6, nchar(date_String))
  date <- as.Date(date, format = "%B %d, %Y")
  return(date)
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
         new = "Away.Team")

dt <- merge(x=dt, y=dt_teamNameMapping, all.x=TRUE)
#dt[, .N, keyby = .(Away.Team, Team.Code)]

# dt[, Date := ]



str(date_2_dateKey(dt[1,2]))

head(dt)
"https://www.basketball-reference.com/boxscores/201710200BRK.html"