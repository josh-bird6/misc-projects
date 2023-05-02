##INTERNATIONALLY


WORLD_unis <- basedata %>%  
  mutate(uni_name = str_remove(data.title, pattern = '<div.*link\">'),
         uni_name = str_sub(uni_name, end = -11),
         uni_name = str_remove(uni_name, pattern = '\\(.*\\)'),
         data.rank_display = str_remove(data.rank_display, pattern = "="),
         COUNTRY = case_when(str_detect(data.country, "China")~"China",
                             str_detect(data.country, "Russia")~"Russian Federation",
                             str_detect(data.country, "Brunei")~"Brunei Darussalam",
                             str_detect(data.country, "Czech")~"Czech Republic",
                             str_detect(data.country, "Turke")~"Turkiye",
                             str_detect(data.country, "Palesti")~"Palestinian Territory",
                             str_detect(data.country, "Bosni")~"Bosnia and Herzegovina",
                             T~data.country)) %>%
           select(uni_name, data.rank_display, COUNTRY, data.city, data.region) %>% 
  distinct(COUNTRY, .keep_all = T) %>%
  mutate(cat = row_number(),
         rank = as.numeric(case_when(cat<83~str_sub(data.rank_display,1,3),
                          T~str_sub(data.rank_display,1,4))),
         group = case_when(rank>0&rank<11~ "1-10",
                           rank>10&rank<51~"11-50",
                           rank>50&rank<101~"51-100",
                           rank>100&rank<501~"101-500",
                           rank>500&rank<999~"501-1,000",
                           T~"1,001 and over")) %>% 
  select(-cat) %>% 
  mutate()
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################

world_shapefile <- read_sf("data/World_shapefile/World_Countries__Generalized_.shp")

country_map <-world_shapefile %>% 
  left_join(WORLD_unis, by="COUNTRY") %>% 
  arrange(rank) %>% 
  filter(!is.na(COUNTRYAFF)) %>% 
  tm_shape()+
  tm_polygons("rank",
              breaks = c(0,11,51,101,501,1001,1401),
              labels = c("1-10","11-50","51-100","101-500","501-1,000","1,001 and over"),
              title = "Placement in world \nuniversity ranking") +
  tm_layout(legend.outside=T,
            legend.position = c("left", "bottom"),
            main.title = "Placement of each nation's highest-ranked university in the QS World University Rankings 2023*", 
            main.title.size=1.5,
            legend.title.size = 1.2,
            legend.text.size=.9,
            asp=0)


  
  
  US_shapefile %>% 
  filter(STATEFP<60) %>% 
  mutate(state= case_when(NAME == "District of Columbia"~"Washington D.C.",
                          T~NAME)) %>% 
  left_join(us_unis, by="state")
