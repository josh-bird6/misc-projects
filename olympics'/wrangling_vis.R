library(tidyverse)


#reading in Tokyo 2020 Medal table (from https://olympics.com/tokyo-2020/olympic-games/en/results/all-sports/medal-standings.htm)
olytable <- read_csv2("olytable.csv")

#reading in IMF WEO data, April 2021 (https://www.imf.org/en/Publications/WEO/weo-database/2021/April/download-entire-database)
poptable <- read_csv("poptable_real.csv") %>% 
  select(Country, `Subject Descriptor`, Units, `2019`) %>% 
  filter(`Subject Descriptor` %in% c("Gross domestic product, current prices", "Gross domestic product per capita, current prices"),
         Units == "U.S. dollars") %>% 
  pivot_wider(names_from = `Subject Descriptor`, values_from = `2019`) %>% 
  ######################Manually inputting most recent Syria data from 2010
  mutate(GDP = ifelse(Country == "Syria", "60.04", `Gross domestic product, current prices`),
         GDP_percapita = ifelse(Country == "Syria", "2806.69", `Gross domestic product per capita, current prices`),
         country = case_when(str_detect(Country, 'United States') ~ 'United States of America',
                             str_detect(Country, 'Taiwan Province of China') ~ 'Chinese Taipei', 
                             str_detect(Country, '\\bChina\\b') ~ "People's Republic of China",
                             str_detect(Country, 'United Kingdom') ~ 'Great Britain',
                             str_detect(Country, 'Russia') ~ 'ROC',
                             str_detect(Country, 'Korea') ~ 'Republic of Korea',
                             str_detect(Country, 'The Bahamas') ~ 'Bahamas',
                             str_detect(Country, 'Hong Kong SAR') ~ 'Hong Kong, China',
                             str_detect(Country, 'Slovak Republic') ~ 'Slovakia',
                             str_detect(Country, 'Moldova') ~ 'Republic of Moldova',
                             str_detect(Country, 'Syria') ~ 'Syrian Arab Republic',
                             str_detect(Country, 'Kyrgyz Republic') ~ 'Kyrgyzstan',
                             TRUE ~ Country)) %>% 
  select(-`Gross domestic product, current prices`, -`Gross domestic product per capita, current prices`, -Country) 
        

joined <- left_join(olytable, poptable, by = "country")%>% 
  #IMF WEO apparently doesn't cover Bermuda/Cuba, and omitting Syria for consistency
  filter(!country %in% c("Bermuda", "Cuba", "Syrian Arab Republic")) %>% 
  mutate(GDP = as.numeric(GDP),
         GDP_percapita = as.numeric(GDP_percapita),
         GDP_real = GDP/1000)
  
#######################################################################################

#visgoldmedal

plota <- ggplot(data = joined, aes(x=gold, y=GDP_real)) +
  geom_point(colour = "#ebcd47") +
  geom_smooth(method = 'lm', se=F) +
  theme_bw()+
  labs(x="", 
       y="", 
       title = "Gold medal count") +
  scale_y_continuous(label = scales::comma,
                     breaks = seq(0,25, 2)) +
  scale_x_continuous(breaks = seq(0,40,1),
                     limits = c(1,39)) +
  geom_text(aes(label = ifelse(gold>9|GDP_real>2, country, ""), hjust =1.03))

plotb <- ggplot(data = joined, aes(x=total, y=GDP_real)) +
  geom_point() +
  geom_smooth(method = 'lm', se=F) +
  theme_bw()+
  labs(x="Number of medals won", 
       y="", 
       title = "Overall medal count") +
  scale_y_continuous(label = scales::comma,
                     breaks = seq(0,25, 2)) +
  scale_x_continuous(breaks = seq(0,120,5)) +
  geom_text(aes(label = ifelse(total>30|GDP_real>2, country, ""), hjust =1.03)) +
  theme(axis.title.x=element_text(size=14))

library(ggpubr)

figure <- ggarrange(plota, 
          plotb, 
          ncol=1)

annotate_figure(figure,
                top = text_grob("Comparing Olympic Medal Counts from Tokyo 2020 and Gross Domestic Product", face = 'bold', size = 16),
                left = text_grob("GDP (trillions of USD)", rot = 90, size = 14),
                bottom = text_grob("Sources: Olympics.com and IMF World Economic Outlook database, April 2021", hjust=1, x=1, face='italic', size = 8))

                
