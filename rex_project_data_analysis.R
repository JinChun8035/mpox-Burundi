library(tidyverse)
library(dplyr)

# uploading data
mpox_cases_raw_unfiltered <- read_csv('afr_cases_as_of_february_2nd.csv')
temperature_burundi_unfiltered <- read_csv('Burundi_temperature.csv')

# filtering data
burundi_only <- mpox_cases_raw_unfiltered %>%
    filter(country == "Burundi") %>%
    arrange(week_end_date)

# computing temperature data so that it's weekly
# index by week
temperature_burundi_unfiltered$seven_day_index <- c(0, rep(1:(nrow(temperature_burundi_unfiltered)-1)%/%7))
# remove irrelevant columns
meteor_bujumbura <- subset(temperature_burundi_unfiltered, 
                           select = -c(name, datetime, 
                                       preciptype:snowdepth, 
                                       severerisk:stations)) 
# remove last row as it only contains one day for that week
meteor_bujumbura <- meteor_bujumbura %>% filter(row_number() <= n()-1)
print("seven day index: 0 for 2024-07-21 to 2024-07-27 inclusive, 
      ending at 2025-01-25 to 2025-02-01 index/week 27")
meteor_bujumbura <- meteor_bujumbura %>%
  group_by(seven_day_index) %>%
  summarise(across(c(tempmax:uvindex), mean),
            .groups = 'drop') %>%
  as.data.frame()
