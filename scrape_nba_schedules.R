#library(rvest)
library(data.table)
library(XML)
library(xml2)

url <- "https://www.basketball-reference.com/leagues/NBA_2018_games-october.html"

html <- xml2::read_html(url)
node <- rvest::html_node(html, "table")
table <- rvest::html_table(node, header = TRUE)


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
