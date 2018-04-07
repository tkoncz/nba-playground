#library(rvest)
library(data.table)
#library(XML)
library(xml2)

base_url <- "https://www.basketball-reference.com/leagues/NBA_xYYYYx_games-xmonthx.html"

complete_url <- function(year, month) {
  
  url <- sub(pattern = "xYYYYx", replacement = year, x = base_url)
  url <- sub(pattern = "xmonthx", replacement = month, x = url)
 
  return(url)
}

html_to_table <- function(url) {
  html <- xml2::read_html(url)
  node <- rvest::html_node(html, "table")
  table <- rvest::html_table(node, header = TRUE)
  
  return(table)
}

years <- c(as.character(2005:2018))
months <- c("september", "october", "november", "december", "january", "february",
            "march", "april", "may", "june")

df_schedules <- NULL
for(y in years) {
  for(m in months) {
    schedules <- NULL
    schedules <- try(html_to_table(complete_url(y, m)), silent= TRUE)
    
    if(exists('schedules') && is.data.frame(get('schedules'))) {
      if(exists('df_schedules') && is.data.frame(get('df_schedules'))) {
        df_schedules <- rbind(df_schedules, schedules)
      } else {
        df_schedules <- schedules
      }
      
      Sys.sleep(1)
      print(paste(y, m, sep= "-"))
    }
  }
}
rm(schedules)

schedules.table <- data.table(df_schedules)
fwrite(schedules.table, file = "schedulesTable.csv")
