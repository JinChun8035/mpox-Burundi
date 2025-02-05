library("tidyverse")

# uploading data
mpox_cases_raw_unfiltered <- read_csv("afr_cases_as_of_february_2nd.csv")
temperature_burundi_unfiltered <- read_csv("Burundi_temperature.csv")

# filtering data
burundi_only <- mpox_cases_raw_unfiltered %>%
    filter(country == "Burundi") %>%
    arrange(week_end_date)

# computing temperature data so that it's weekly
