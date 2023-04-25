
library(httr)
library(tidyverse)

#https://www.reddit.com/r/webscraping/comments/ns1l8m/web_scraping_qs_data_with_rvest/

# F12 -> network -> fetch/xhr

#grabbing text file from Fetch/XHR settings in Network 
basetextfile <- GET("https://www.topuniversities.com/sites/default/files/qs-rankings-data/en/3816281.txt?rtnxp1")

#extracting content in JSON format 
data_json <- content(basetextfile, encoding = "UTF-8")

#converting to Rformat
dataframe <- jsonlite::fromJSON(data_json)

#BASE DATASET
basedata <- data.frame(dataframe)
#################################################