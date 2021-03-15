library(tidyverse)
library(lubridate)
library(treemapify)


basedata <- read.csv("liftin'.csv") %>% 
  select(date, day, workout, weight) %>% 
  mutate(date2 = dmy(date),
         year = year(date2),
         month = month(date2),
         day2 = day(date2)) %>%  
  select(date2, day2, month, year, day, workout, weight)

basedata$date2 <- as.Date(basedata$date2)


#vis
##By day
dayz <- basedata %>% 
  group_by(day) %>% 
  summarise(total = n())

dayz$day <-
  factor(
    dayz$day,
    levels = c(
      " Monday",
      " Tuesday",
      " Wednesday",
      " Thursday",
      " Friday",
      " Saturday",
      " Sunday"
    )
  )

##################PROBABLY THE SECOND
ggplot(data = dayz, aes(x = day, y = total)) +
  geom_col(fill = "#9ecae1") +
  theme_minimal() +
  geom_text(aes(label = total), vjust=-.5) +
  labs(title = "Fig 1: Workout totals by day", x="", y="Total")

###############PRobably third
##by workout
basedata %>% 
  group_by(workout) %>% 
  summarise(Total = n()) %>% 
  ggplot(aes(x=reorder(workout,Total), y=Total)) +
  geom_col(fill="#9ecae1") +
  theme_minimal() +
  coord_flip() +
  geom_text(aes(label = Total), hjust=1) +
  labs(title = "Workout totals by type", x="")

#TREEMAP
basedata %>% 
  group_by(workout) %>% 
  summarise(Total = n()) %>% 
  mutate(totals= sum(Total),
         overall_total = as.numeric(str_sub(Total/totals*100, 1,4))) %>% 
  ggplot(aes(area=overall_total, fill = overall_total, label = workout, subgroup = paste0(Total, paste0('(',overall_total,'%',')')))) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre") +
  geom_treemap_subgroup_text(place = "topleft", colour = "white", fontface = "italic", size = 11) +
  guides(fill=F)
  


##weight
ggplot(data = basedata, aes(x=date2, y=weight)) +
  geom_point(fill = "#9ecae1") + 
  geom_smooth(method = "lm", se=F, color = "#9ecae1") +
  theme_minimal() +
  scale_x_date(date_labels="%b %y", date_breaks ="1 month") +
  scale_y_continuous(limits = c(75, 80)) +
  labs(y="Weight(kg)", x = "Month", title = "Fig 3: Weight by month")


#And next
test <- basedata %>% 
  unite_("monthyear", sep="-", c("month", "year")) %>% 
  mutate(monthyear = as.factor(monthyear)) %>% 
  group_by(monthyear, workout) %>% 
  summarise(total = n())
  
test$monthyear <-
  factor(
    test$monthyear,
    levels = c(
      "7-2018",
      "8-2018",
      "9-2018",
      "10-2018",
      "11-2018",
      "12-2018",
      "1-2019",
      "2-2019",
      "3-2019",
      "4-2019",
      "5-2019",
      "6-2019",
      "7-2019"
    )
  )

####################PROBABLY THE FIRST 
ggplot(data = test, aes(x = monthyear, y = total)) +
  geom_col(fill = "#9ecae1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Number of workouts by month", x = "Month", y="Total") 
 


ggplot(data = test, aes(x = monthyear, y = total)) +
  geom_col(fill = "#9ecae1") +
  theme_minimal() +
  facet_wrap(~workout, nrow = 2) +
  theme(axis.text.x = element_text(angle = 90, vjust=.4),
        strip.background = element_rect(fill = "#9ecae1"),
        panel.border = element_rect(color="black", fill=NA)) +
  labs(title = "Workouts by month", x = "Month", y="Total") 
#########

test2 <- basedata %>% 
  unite_("monthyear", sep="-", c("month", "year")) %>% 
  mutate(monthyear = as.factor(monthyear)) %>% 
  group_by(monthyear, day) %>% 
  summarise(total = n())

test2$monthyear <-
  factor(
    test2$monthyear,
    levels = c(
      "7-2018",
      "8-2018",
      "9-2018",
      "10-2018",
      "11-2018",
      "12-2018",
      "1-2019",
      "2-2019",
      "3-2019",
      "4-2019",
      "5-2019",
      "6-2019",
      "7-2019"
    )
  )

test2$day <-
  factor(
    test2$day,
    levels = c(
      " Monday",
      " Tuesday",
      " Wednesday",
      " Thursday",
      " Friday",
      " Saturday",
      " Sunday"
    )
  )

ggplot(data=test2, aes(x=day, y=total)) +
  geom_col(fill="#9ecae1") +
  theme_minimal() +
  facet_wrap(~monthyear) +
  theme(axis.text.x = element_text(angle = 90, vjust=.4),
        strip.background = element_rect(fill = "#9ecae1"),
        panel.border = element_rect(color="black", fill=NA))

  
