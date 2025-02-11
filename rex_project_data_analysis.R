library(tidyverse)
library(dplyr)

# uploading data
mpox_cases_raw_unfiltered <- read_csv('afr_cases_as_of_february_2nd.csv')
temperature_burundi_unfiltered <- read_csv('correct_metereological_data.csv')

# filtering data
burundi_cases <- mpox_cases_raw_unfiltered %>%
  filter(country == "Burundi") %>%
  subset(select = c(week_end_date:total_suspected_cases, 
                    new_confirmed_cases:new_suspected_cases)) %>%
  arrange(week_end_date)
burundi_cases$ID <- c(0: (nrow(burundi_cases) - 1))
temperature_burundi_unfiltered$ID <- c(0, rep(1:(nrow(temperature_burundi_unfiltered)-1)%/%7))
meteor_bujumbura <- temperature_burundi_unfiltered %>%
  subset(select = -c(name, datetime, feelslikemax:feelslike, 
                     preciptype:snowdepth, severerisk:stations)) %>%
  group_by(ID) %>%
  summarise(across(c(tempmax:uvindex), mean), .groups = 'drop') %>%
  as.data.frame()

# match variables to existing papers
meteor_bujumbura <- subset(meteor_bujumbura, 
                           select = c(ID, temp, dew:precip, windspeed))

# merge datasets
inner_join <- merge(meteor_bujumbura, burundi_cases, by = "ID")

