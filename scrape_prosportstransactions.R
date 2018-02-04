#library(rvest)
library(data.table)
library(XML)

base_url <- 'http://www.prosportstransactions.com/basketball/Search/SearchResults.php?Player=&Team=&BeginDate=2000-10-01&EndDate=&ILChkBx=yes&Submit=Search&start='

url <- paste(base_url, as.character(0), sep = "")
df_injuries = readHTMLTable(url, header = T, which = 1, stringsAsFactors = F)

for(i in seq(from= 25,to= 22875,by= 25)) {
  
  url <- paste(base_url, as.character(i), sep = "")
  injuries = readHTMLTable(url, header = T, which = 1, stringsAsFactors = F)
  
  df_injuries <- rbind(df_injuries, injuries)
  rm(injuries)
  print(i) #tracking progress
}

# removing variables not used anymore
rm(base_url)
rm(url)
rm(i)

injury.table <- data.table(df_injuries)
rm(df_injuries)

head(injury.table)

str_to_replace <- substring(injury.table[5, 4], 1, 2)

injury.table[, Acquired := gsub(pattern = str_to_replace, replacement = "", x = Acquired)]
injury.table[, Relinquished := gsub(pattern = str_to_replace, replacement = "", x = Relinquished)]

fwrite(injury.table, file = "injuriesTable.csv")
