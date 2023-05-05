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
                           T~"1,001+")) %>% 
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

library(tmap)
library(sf)

#map

world_shapefile <- read_sf("data/World_shapefile/World_Countries__Generalized_.shp")

country_map <-tmap_grob(world_shapefile %>% 
  left_join(WORLD_unis, by="COUNTRY") %>% 
  arrange(rank) %>% 
  filter(!is.na(COUNTRYAFF),
         COUNTRY != "Antarctica") %>% 
  tm_shape()+
  tm_polygons("rank",
              breaks = c(0,11,51,101,501,1001,1401),
              labels = c("1-10","11-50","51-100","101-500","501-1,000","1,001+"),
              title = "Placement in world \nuniversity ranking") +
  tm_layout(legend.outside=T,
            legend.position = c("left", "bottom"),
            main.title = "Placement of each nation's highest-ranked university in the QS World University Rankings 2023*", 
            # main.title.size=2,
            # legend.title.size =3,
            # legend.text.size=1.8,
            main.title.size=2,
            legend.title.size = 1.2,
            legend.text.size=1.2,
            asp = 1.7) +
  tm_credits("*No universities from greyed-out countries appeared in the rankings", 
             position = c("LEFT", "BOTTOM"), 
             size = 1))

country_map
################

country_chart <- WORLD_unis %>% 
  filter(COUNTRY != "") %>% 
  ggplot(aes(x=reorder(COUNTRY,-rank), y=rank, fill = group))+
  geom_col()+
  geom_text(aes(label = paste0(uni_name, " (",data.rank_display,")")), hjust=-.01) +
  theme_bw() +
  coord_flip() +
  labs(x="",
       y="World University Ranking") +
  scale_y_continuous(limits = c(0, 1800), expand = c(0, 0),
                     breaks = seq(0,1400, by=500),
                     labels = scales::comma) +
  scale_fill_manual(values = c("#ffffd4", "#993404","#fe9929","#fee391" ,"#d95f0e", "#fec44f")) +
  theme(legend.position = "none",
        plot.caption = element_text(size = 18, hjust = 0),
        axis.title = element_text(size=15),
        axis.text = element_text(colour = "black", size=12),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

# 
# cowplot::plot_grid(country_map,  country_chart, ncol=1) 

#2000x2700
output <- cowplot::plot_grid(country_map, NULL, country_chart, ncol=1, rel_heights = c(1,-.15,1))

cowplot::ggsave2("WORLDOUTPUT_Final.png", output, width=529.16666667, height =714.375, units = "mm", dpi=300)
  
