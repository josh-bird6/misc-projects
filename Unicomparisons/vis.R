##grabbing shapefile: https://catalog.data.gov/dataset/tiger-line-shapefile-2019-nation-u-s-current-county-and-equivalent-national-shapefile

library(tmap)
library(sf)

##MAP
#zooming in
 US_shapefile <- read_sf("data/US_shapefile/cb_2018_us_state_5m.shp") %>% 
  tigris::shift_geometry()

 
 test1<-tmap_grob(US_shapefile %>% 
  filter(STATEFP<60) %>% 
  mutate(state= case_when(NAME == "District of Columbia"~"Washington D.C.",
                          T~NAME)) %>% 
  left_join(us_unis, by="state") %>% 
  tm_shape()+
  tm_polygons("rank",
              breaks = c(0,11,51,101,501,1001,1400),
              labels = c("1-10","11-50","51-100","101-500","501-1,000","1,001 and over"),
              title = "Placement in world \nuniversity ranking") +
  tm_layout(legend.outside=T,
            legend.position = c("left", "bottom"),
            main.title = "Placement of each state's highest-ranked university in the QS World University Rankings 2023*", 
            main.title.size=1.5,
            legend.title.size = 1.2,
            legend.text.size=.9,
            asp=0))

##GRAPH

test2 <- us_unis %>% 
  ggplot(aes(x=reorder(state,-rank), y=rank, fill = group))+
  geom_col()+
  geom_text(aes(label = paste0(uni_name.x, " (",data.rank_display,")")), hjust=-.01) +
  theme_bw() +
  coord_flip() +
  labs(x="",
       y="World University Ranking",
       caption = "*Note: No universities from Maine, Nevada, North Dakota or South Dakota were included in the rankings \nUniversities placed 500 and lower are ranked in bands rather than individually") +
  scale_y_continuous(limits = c(0, 1450), expand = c(0, 0),
                     labels = scales::comma) +
  scale_fill_manual(values = c("#ffffd4", "#993404","#fe9929","#fee391" ,"#d95f0e", "#fec44f")) +
  theme(legend.position = "none",
        plot.caption = element_text(size = 15, hjust = 0),
        axis.text = element_text(colour = "black", size=11),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
  

#1200x1500 pixels
cowplot::plot_grid(test1, test2, ncol=1) 


