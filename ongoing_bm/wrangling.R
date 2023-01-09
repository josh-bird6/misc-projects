library(tidyverse)
library(readxl)


basedata <- read_excel("schedule.xlsx") %>% 
  mutate(date = as.Date(date),
         month = lubridate::month(lubridate::ymd(date), label = T, abbr = F),
         month2 = lubridate::month(lubridate::ymd(date), label = F),
         day= lubridate::wday(date, label =T, abbr = F))



#Cumulative dumps
basedata %>% 
  ggplot(aes(x=date, y=cumsum(status)))+
  geom_line()+
  theme_bw() +
  scale_y_continuous(breaks = seq(0,180, by = 10),
                     expand = c(0,3)) +
  labs(title = "Cumulative dumps, 8 January 2022 to 8 January 2023",
       x="",
       y="Total") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")+
  annotate("text", x = as.Date("2022-12-07"), y= 170, label = "Closing annual total: 157") +
  annotate("text", x = as.Date("2022-06-13"), y= 150, label = "Tenerife")+
  annotate("text", x = as.Date("2022-08-16"), y= 130, label = "Georgia")+
  annotate("text", x = as.Date("2022-12-15"), y= 110, label = "Roma")+
  annotate("segment", x=as.Date("2023-01-08"),xend=as.Date("2023-01-08"), y=165, yend = 157,size = .9) +
  annotate("rect", xmin=as.Date("2022-06-29"),xmax=as.Date("2022-07-04"), ymin= 0, ymax=170, fill = "gray", alpha = .6) +
  annotate("rect", xmin=as.Date("2022-08-31"),xmax=as.Date("2022-09-12"), ymin= 0, ymax=170, fill = "gray", alpha = .6) +
  annotate("rect", xmin=as.Date("2022-12-27"),xmax=as.Date("2022-12-30"), ymin= 0, ymax=170, fill = "gray", alpha = .6)



#Dumps by day
basedata %>% 
  group_by(day) %>% 
  summarise(total = sum(status)) %>% 
  ggplot(aes(x=day, y=total))+
  geom_col()+
  theme_bw() +
  labs(title = "Dumps by day, 2022",
       x = "",
       y="Total") +
  geom_text(aes(label = total),vjust=-.1)

#dumps by month
basedata %>% 
  group_by(month, month2) %>% 
  summarise(total = sum(status)) %>% 
  ggplot(aes(x=month, y=total))+
  geom_point()+
  geom_smooth(aes(x=month2), method="lm", se=F)+
  theme_bw() +
  labs(title = "Dumps by month, 2022",
       x = "",
       y="Total") +
  scale_y_continuous(breaks = seq(0,20, by = 1),
                     limits = c(0,20, by = 1))

#gaps?
basedata %>% 
  mutate(test = cumsum(status)) %>% 
  group_by(test) %>% 
  summarise(total = n()-1) %>% 
  ggplot(aes(x=total)) +
  geom_histogram(bins = 6, col=I("black")) +
  scale_x_continuous(breaks = seq(0,6, by=1)) +
  labs(title = "Gaps in the bowel",
       x="Number of days in between dumps",
       y="Total") +
  theme_bw()
