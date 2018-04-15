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
      schedules$Season <- paste(as.character(as.integer(y)-1), as.character(y), sep = "-")
        
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

schedules_table <- data.table(df_schedules)

schedules_table <- schedules_table %>%
                     setnames(old = 1:6, new = c("Date",      "Start_ET", 
                                                 "Away.Team", "Away.Points", 
                                                 "Home.Team", "Home.Points")) %>%
                     select(Season, Date, Start_ET, Away.Team, Away.Points, Home.Team, Home.Points)

schedules_table <- schedules_table %>%
                     filter(Date != "Playoffs") %>%
                     tidyr::separate(col = Date, into = c("Day.Of.Week", "Month.Day", "Year"), sep = ", ", fill = "right") %>%
                     mutate(Date = paste(Year, Month.Day, sep = " ")) %>%
                     mutate(Date = ymd(Date)) %>%
                     mutate(Away.Points = as.integer(Away.Points)) %>%
                     mutate(Home.Points = as.integer(Home.Points)) %>%
                     filter(!is.na(Away.Points)) %>%
                     filter(!is.na(Home.Points)) %>%
                     select(Season, Date, Start_ET, Away.Team, Away.Points, Home.Team, Home.Points)

last_day_of_reg <- fread("C:/Users/tkonc/Documents/Data/NBA Playground/LastDayOfRegularSeason.csv")

schedules_table %>% filter(Season == "2017-2018") %>% tail()

fwrite(schedules_table, file = "schedulesTable.csv")
