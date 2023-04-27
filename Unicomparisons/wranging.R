
library(httr)
library(tidyverse)


#https://www.reddit.com/r/webscraping/comments/ns1l8m/web_scraping_qs_data_with_rvest/
#https://simplemaps.com/data/world-cities

# F12 -> network -> fetch/xhr

#grabbing text file from Fetch/XHR settings in Network 
basetextfile <- GET("https://www.topuniversities.com/sites/default/files/qs-rankings-data/en/3816281.txt?rtnxp1")

#extracting content in JSON format 
data_json <- content(basetextfile, encoding = "UTF-8")

#converting to R format
dataframe <- jsonlite::fromJSON(data_json)

#BASE DATASET
basedata <- data.frame(dataframe)
#################################################

#pulling in city/state deets from: #https://public.opendatasoft.com/explore/dataset/us-colleges-and-universities/export/

# stateinfo <- read_csv("data/worldcities.csv") %>% 
#   mutate(state = admin_name,
#          data.city = case_when(city_ascii == "New York"~"New York City", 
#                                T~city_ascii)) %>% 
#   filter(country == "United States") %>% 
#   select(5,12,13)



citystate_info <- read_csv("data/us_unis.csv") %>% 
  filter(NAICS_DESC == "COLLEGES, UNIVERSITIES, AND PROFESSIONAL SCHOOLS") %>% 
  mutate(state = usdata::abbr2state(STATE),
         data.city = str_to_title(CITY),
         uni_name = str_to_title(NAME),
         ############################# MANUAL WRANGLING THAT IS AWFUL
         data.city = case_when(data.city=="New York"~ "New York City", 
                               T~data.city),
         data.city = case_when(data.city == "University"~ "Oxford", 
                               T~data.city),
         data.city = str_replace(data.city, "Saint", "St."),
         uni_name = case_when(str_detect(uni_name, "Georgia")~uni_name, 
                              T~str_replace(uni_name, "In ", "in ")),
         uni_name = case_when(str_detect(uni_name, "Ohio State")~"The Ohio State University",
                              T~uni_name),
         uni_name = case_when(str_detect(uni_name, "San Diego")~"University of California, San Diego",
                              T~uni_name),
         uni_name = str_replace(uni_name, "A & M", "A&M"),   
         uni_name = str_replace(uni_name, "Of", "of"),                                                 ### fixing the 'of' case
         uni_name = str_replace(uni_name,"The University o", "University o"),
         MATCHVAR = str_sub(uni_name, 1,17)) %>% 
  select(uni_name, data.city, state, MATCHVAR)

#################################################



#wrangling the base dataset

#jfc that was annoying
# https://www.dataquest.io/blog/install-package-r/
                 
us_unis <- basedata %>% 
  filter(data.country == "United States") %>% 
  mutate(uni_name = str_remove(data.title, pattern = '<div.*link\">'),
         uni_name = str_sub(uni_name, end = -11),
         uni_name = str_remove(uni_name, pattern = '\\(.*\\)'),
         uni_name = case_when(uni_name == "The Ohio State University" ~ uni_name,
                              T~str_replace(uni_name,"The University o", "University o")),
         MATCHVAR = str_sub(uni_name, 1,17),
         data.rank_display = str_remove(data.rank_display, pattern = "=")) %>% 
  select(data.city, data.score, data.rank_display, uni_name, MATCHVAR) %>% 
  left_join(citystate_info, by=c("data.city", "MATCHVAR")) %>% 
  mutate(state = case_when(str_detect(uni_name.x, "Montana")~"Montana",
                           T~state),
         state = case_when(str_detect(uni_name.x, "Georgetown")~"Washington D.C.",
                           T~state)) %>% 
  distinct(uni_name.x, .keep_all = T) %>% 
  select(-MATCHVAR, -uni_name.y) %>% 
  distinct(state, .keep_all =T) %>% 
  filter(!is.na(state)) %>% 
  mutate(rank = case_when(str_detect(data.rank_display, "-")~str_sub(data.rank_display, 1,3), 
                          T~data.rank_display),
         rank = as.numeric(rank),
         rank = case_when(state %in% c("Arkansas", "Montana", "West Virginia")~rank+900,
                          T~rank),
         group = case_when(rank >0 & rank<11~"1-10",
                           rank >10 & rank<51~"11-50",
                           rank >50 & rank<101~"51-100",
                           rank >100 &rank<501~"101-500",
                           rank >500 & rank<1000~"501-1,000",
                           T~"1,001 and over"))

#########################################
