library(tidyverse) 

#reading in base set and converting NAs to 0
basedata <- read_csv("prez.csv") %>% 
  mutate_all(funs(replace_na(., 0))) 

#Getting electoral vote totals
basedata_electoral <- basedata %>%
  group_by(state) %>% 
  summarise(overall_electoral = sum(Trump_electoral + Clinton_electoral + Other_electoral)) 

#merging back to original dataset
basedata$overall_electoral <- basedata_electoral$overall_electoral[(match(basedata$state, basedata_electoral$state))]


#Getting popular vote percentages and using that to determine actual 

basedata_finalised <- basedata %>% 
  mutate(Trump_popular_percent = round(Trump_popular/TOTAL_POP_VOTE, 10),
         Clinton_popular_percent = round(Clinton_popular/TOTAL_POP_VOTE, 10),
         Other_popular_percent = round(Others_popular/TOTAL_POP_VOTE, 10),
         Trump_electoral_ACTUAL = round(Trump_popular_percent*overall_electoral),
         Clinton_electoral_ACTUAL = round(Clinton_popular_percent*overall_electoral),
         Other_electoral_ACTUAL= round(Other_popular_percent*overall_electoral),
         Clinton_percent = round(Clinton_popular_percent*100, 1),
         Trump_percent = round(Trump_popular_percent*100, 1),
         Other_percent = round(Other_popular_percent*100, 1),
         difference = Trump_percent - Clinton_percent,
         overall_electoral = ifelse(state %in% c("HI", "KS", "MA", "MI", "MS", "MO", "OR", "TN", "UT", "WA", "WV", "WI"), 
                                    paste0(overall_electoral, "*"), overall_electoral)) # adding an asterisk for recallibrated electoral vote totals
  
basedata_vis <- basedata_finalised %>% 
  gather(key = "category", value = "electoral_votes_real", Trump_electoral_ACTUAL:Other_electoral_ACTUAL)


ggplot(data = basedata_vis, aes(x=reorder(state, overall_electoral), y=electoral_votes_real, fill = category)) +
  geom_col(position = "dodge") +
  coord_flip() +
  theme_minimal()

#####################################################################################################
