library(shiny)
library(shinydashboard)
library(networkD3)
library(tidyverse)


#############################################
NAD_overall <- read.csv("M:/NAD Development/_sas_datadump/nad_for_slicers.csv") %>% 
  select(
    in_nad,
    h_ay2,
    articulation,
    schlname_post_merge,
    f_cotitle,
    h_cotitle,
    he_institution,
    h_gender,
    f_qual,
    final_hn_level,
    FT,
    subj,
    ethgrp3,
    jacs1_exp,
    age_band,
    simd_quint,
    NAD_Countable) %>%  
  mutate(he_institution=str_replace_all(he_institution, "Scotland's Rural College", ""))
# %>% write.csv("M:/Performance Measurement & Analysis/Josh/NAD/NAD_sankey.csv")

#####################################
#Node list
nodes <- data.frame("name" = c("Argyll College  (UHI)",
                               "Ayrshire College",
                               "Borders College",
                               "City of Glasgow College",
                               "Dumfries and Galloway College",
                               "Dundee and Angus College",
                               "Edinburgh College",
                               "Fife College",
                               "Forth Valley College",
                               "Glasgow Clyde College",
                               "Glasgow Kelvin College",
                               "Inverness College (UHI)",
                               "Lews Castle College (UHI)",
                               "Moray College (UHI)",
                               "New College Lanarkshire",
                               "Newbattle Abbey College",
                               "North East Scotland College",
                               "North Highland College (UHI)",
                               "Orkney College (UHI)",
                               "Perth College (UHI)",
                               "Scotland's Rural College (SRUC)",
                               "Shetland College (UHI)",
                               "South Lanarkshire College",
                               "UHI college (unknown)",
                               "West College Scotland",
                               "West Highland College (UHI)",
                               "West Lothian College",
                               "Aberdeen University",
                               "Abertay University",
                               "Dundee University",
                               "Edinburgh University",
                               "Glasgow Caledonian University",
                               "Glasgow School of Art",
                               "Glasgow University",
                               "Heriot-Watt University",
                               "Napier University",
                               "Non-Scottish University",
                               "Open University",
                               "Queen Margaret University",
                               "Royal Conservatoire of Scotland",
                               " (SRUC)",
                               "St Andrew's University",
                               "Stirling University",
                               "Strathclyde University",
                               "The Robert Gordon University",
                               "University of the Highlands and Islands",
                               "University of the West of Scotland")) %>% 
  mutate(ID=row_number(),
         ID_real=ID-1) %>% 
  select(-ID)
######################################################

#Adding to edge list 
NAD_overall <- left_join(NAD_overall, nodes, by=c("schlname_post_merge"="name"))
NAD_overall <- left_join(NAD_overall, nodes, by=c("he_institution"="name"))

#renaming and formatting
NAD_overall <- NAD_overall %>%
  rename(source = ID_real.x, target = ID_real.y) %>%
  select(
    in_nad,
    h_ay2,
    articulation,
    schlname_post_merge,
    source,
    he_institution,
    target,
    everything()
  )

#############################################################################

#SHINYAPP

# ui <- dashboardPage(
#   dashboardHeader(title = "Basic dashboard"),
#   dashboardSidebar(
#     htmlOutput("year")),
#   
#   dashboardBody(
    # Boxes need to be put in a row (or column)

ui <- shinyUI(
  fluidPage(
    titlePanel("Reactive select input boxes"),
    sidebarPanel(
      htmlOutput("year")
    ),      
    mainPanel(
      sankeyNetworkOutput("plot1"))
    )
)

server <- function(input, output) {
  
  #Year selector
  output$year = renderUI({
    selectInput(inputId="year",
                label="Year",
                choices = as.character(unique(NAD_overall$h_ay2)),
                selected = "2017-18")
  })
  
  #THE ACTUAL PLOT
  output$plot1 <- renderSankeyNetwork({
    sankeyNetwork(Links = NAD_overall, Nodes = nodes,
                  Source = "source", Target = "target",
                  Value = "in_nad", 
                  fontSize= 12, nodeWidth = 30)
  })
}

shinyApp(ui, server)


