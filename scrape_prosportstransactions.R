#library(rvest)
library(data.table)
library(XML)

#Specifying the url for desired website to be scrapped
base_url <- 'http://www.prosportstransactions.com/basketball/Search/SearchResults.php?Player=&Team=&BeginDate=2000-10-01&EndDate=&ILChkBx=yes&Submit=Search&start='

url <- paste(base_url, as.character(0), sep = "")
df_injuries = readHTMLTable(url, header = T, which = 1, stringsAsFactors = F)

for(i in seq(from= 25,to= 100,by= 25)) {
  
  url <- paste(base_url, as.character(i), sep = "")
  injuries = readHTMLTable(url, header = T, which = 1, stringsAsFactors = F)
  
  df_injuries <- rbind(df_injuries, injuries)
}

?seq
