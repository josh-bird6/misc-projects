
library(leaflet)
library(rgdal)
library(tmaptools)

states <- readOGR("M:/Performance Measurement & Analysis/Josh/Misc/prez/tl_2019_us_state/tl_2019_us_state.shp",
                  layer = "tl_2019_us_state",
                  GDAL1_integer64_policy = T)

#converting to WGS84 and simplifying
wgs_states <- spTransform(states, CRS("+proj=longlat +datum=WGS84"))
simplified_wgs_states <- rmapshaper::ms_simplify(wgs_states)


#REMOVING THE PLACES I WON'T BE USING
simplified_wgs_states <- simplified_wgs_states[simplified_wgs_states$NAME != "United States Virgin Islands" , ]
simplified_wgs_states <- simplified_wgs_states[simplified_wgs_states$NAME != "Commonwealth of the Northern Mariana Islands", ]
simplified_wgs_states <- simplified_wgs_states[simplified_wgs_states$NAME != "Puerto Rico", ]
simplified_wgs_states <- simplified_wgs_states[simplified_wgs_states$NAME != "Guam", ]
simplified_wgs_states <- simplified_wgs_states[simplified_wgs_states$NAME != "American Samoa", ]

#Joining
simplified_wgs_states$difference <- basedata_finalised$difference[match(simplified_wgs_states$STUSPS, basedata_finalised$state)]
simplified_wgs_states$overal_electoral <- basedata_finalised$overall_electoral[match(simplified_wgs_states$STUSPS, basedata_finalised$state)]
simplified_wgs_states$Trump_percent <- basedata_finalised$Trump_percent[match(simplified_wgs_states$STUSPS, basedata_finalised$state)]
simplified_wgs_states$Clinton_percent <- basedata_finalised$Clinton_percent[match(simplified_wgs_states$STUSPS, basedata_finalised$state)]
simplified_wgs_states$Other_percent <- basedata_finalised$Other_percent[match(simplified_wgs_states$STUSPS, basedata_finalised$state)]
simplified_wgs_states$Trump_electoral_actual <- basedata_finalised$Trump_electoral_ACTUAL[match(simplified_wgs_states$STUSPS, basedata_finalised$state)]
simplified_wgs_states$Clinton_electoral_actual <- basedata_finalised$Clinton_electoral_ACTUAL[match(simplified_wgs_states$STUSPS, basedata_finalised$state)]
simplified_wgs_states$Other_electoral_actual <- basedata_finalised$Other_electoral_ACTUAL[match(simplified_wgs_states$STUSPS, basedata_finalised$state)]


#creating diverging colour palette

reds <- colorRampPalette(colors = c("#fcbba1", "#a50f15"), space = "Lab")(47)
blues <- colorRampPalette(colors = c("#08306b", "#c6dbef"), space = "Lab")(88)

colorpalette <- c(blues, reds)

pal <- colorNumeric(
  palette = colorpalette,
  domain = simplified_wgs_states$difference
)

########mapping

labels <- sprintf(
  "<font size = 4>%s</font><br/>
  <font size = 2><strong>Number of electoral votes: %s</strong><br/>
  Trump popular vote share: %s<br/>
  Clinton popular vote share: %s<br/>
  Other popular vote share: %s <br/><br/>
  <strong> Trump amended electoral tally: %s<br/>
  Clinton amended electoral tally: %s<br/>
  Other amended electoral tally: %s</strong></font>",
  simplified_wgs_states$NAME,
  simplified_wgs_states$overal_electoral,
  paste0(simplified_wgs_states$Trump_percent, "%"),
  paste0(simplified_wgs_states$Clinton_percent, "%"),
  paste0(simplified_wgs_states$Other_percent, "%"),
  simplified_wgs_states$Trump_electoral_actual,
  simplified_wgs_states$Clinton_electoral_actual,
  simplified_wgs_states$Other_electoral_actual) %>%
  lapply(htmltools::HTML)


##vis
 basedata_finalised %>% 
  leaflet() %>% 
  addProviderTiles('Esri.WorldShadedRelief') %>% 
  addPolygons(
    color = "#444444",
    data = simplified_wgs_states,
    weight = 1,
    smoothFactor = 0.5,
    opacity = 1.0,
    fillOpacity = 100,
    fillColor = ~pal(simplified_wgs_states$difference),
    label = labels,
    highlightOptions = highlightOptions(
      color = "white",
      weight = 2,
      bringToFront = T)) %>% 
  addLegend("bottomright", 
            pal = pal,
            values = ~simplified_wgs_states$difference,
            title = "Difference in popular vote share,<br/> 2016 US presidential election",
            labFormat = labelFormat(suffix = "%"),
            opacity = 1)


#exporting
library(mapview)
mapshot(export, url = "ex1.html")

