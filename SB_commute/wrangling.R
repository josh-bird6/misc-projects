library(tidyverse)
library(readxl)

basedata <- read_excel('Commute.xlsx') %>% 
  data.frame() %>%
  mutate(traveltime = as.numeric(Arrive.time - Depart.time),
         delaytime = readr::parse_number(Notes),
         actualtravel = case_when(is.na(delaytime) ~ traveltime,
                                  T~ traveltime + delaytime),
         dayofweek = wday(Date, label = T)) %>% 
  filter(!is.na(actualtravel))  %>% 
  mutate(monthabb = format(Date, "%Y-%m")) %>% 
  filter(actualtravel <180)


summary(basedata$actualtravel)

#histogram
basedata %>% 
  ggplot(aes(x=actualtravel)) +
  geom_histogram(bins = 35) +
  facet_wrap(~Arrive.location) +
  labs(x = "travel time (mins)",
       y="",
       title = "Distribution of travel times buy destination") +
  theme_bw() +
  scale_x_continuous(limits = c(0,200), breaks = seq(0,200, 20)) +
  geom_vline(data = . %>% group_by(Arrive.location) %>% summarise(actualtravel = mean(actualtravel)), aes(xintercept = actualtravel))

#by day
basedata %>% 
  group_by(dayofweek, Arrive.location) %>% 
  summarise(test = mean(actualtravel)) %>% 
  ggplot(aes(x=dayofweek, y=test, group = Arrive.location, fill = Arrive.location))+
  geom_col(position = "dodge")

#by month
basedata %>% 
  group_by(monthabb, Arrive.location) %>% 
  summarise(test = mean(actualtravel)) %>% 
  ggplot(aes(x = monthabb, y = test, group = Arrive.location, color = Arrive.location)) +
  geom_point()+
  geom_line()+
  scale_y_continuous(limits = c(0,100))+
  labs(x="",
       y="Average travel time (mins)",
       title = "Average travel time by month, August 2023 - June 2024") +
  theme_bw()


library(GGally)

basedata %>% 
  select(actualtravel, Arrive.location, dayofweek, monthabb) %>% 
  ggpairs()
  
summary(lm(actualtravel ~ Arrive.location + dayofweek +  monthabb, data = basedata))
