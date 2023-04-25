
library(httr)
library(tidyverse)

#https://www.reddit.com/r/webscraping/comments/ns1l8m/web_scraping_qs_data_with_rvest/

# F12 -> network -> fetch/xhr

response <- GET("https://www.topuniversities.com/sites/default/files/qs-rankings-data/en/3816281.txt?rtnxp1")

data_json <- content(response, encoding = "UTF-8")

dataframe <- jsonlite::fromJSON(data_json)

df <- data.frame(dataframe) %>% 
  filter(data.country == "United States")
