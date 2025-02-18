library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(corrplot)
library(MASS)
library(pscl)

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

# making date as date type data
inner_join <- inner_join %>%
  mutate(week_end_date = as.Date(week_end_date))

# creating another data frame to manipulate data for more efficient
# computation by making data longer
longer_data <- inner_join %>%
  pivot_longer(cols = c("temp", "dew", "humidity", "precip", "windspeed"),
               names_to = "metereological_factor",
               values_to = "data")

# Inspect Data
str(inner_join)  # Check structure of dataset
summary(inner_join)  # Summary statistics

# Descriptive Statistics
descriptive_table <- inner_join %>% summarise(
  temp_mean = mean(temp), temp_sd = sd(temp),
  humidity_mean = mean(humidity), humidity_sd = sd(humidity),
  dew_mean = mean(dew), dew_sd = sd(dew),
  precip_median = median(precip), precip_iqr = IQR(precip),
  windspeed_mean = mean(windspeed), windspeed_sd = sd(windspeed),
  new_cases_mean = mean(new_confirmed_cases), new_cases_sd = sd(new_confirmed_cases)
)
print(descriptive_table)

# 2a Identifying time-series trends 
# Plot: Line chart (new_confirmed_cases vs. week_end_date)

plot_newcases_end_date <- inner_join %>%
  ggplot(aes(x=week_end_date, y=new_confirmed_cases)) +
  geom_line() +
  geom_point() +
  labs(title="Burundi New Confirmed Clade1b Mpox Cases vs Date of Data 
       Collection, weekly",
       x= "Date of Data Collection", 
       y= "Newly Confirmed Clad1b Mpox Cases")
plot_newcases_end_date

# Major peak identified in November
# Small peaks identified in September and late January


# Plotting environmental factors over time (checking climate fluctuations)

plot_env_time <- ggplot(inner_join, aes(x = week_end_date)) +
  geom_line(aes(y = temp, color = "Temperature")) +
  geom_line(aes(y = humidity, color = "Humidity")) +
  geom_line(aes(y = precip, color = "Precipitation")) +
  geom_line(aes(y = windspeed, color = "Wind Speed")) +
  geom_line(aes(y = dew, color = "Dew")) + 
  labs(title = "Environmental Trends Over Time", x = "Week", y = "Value")

plot_env_time

plot_env_time_2 <- ggplot(longer_data, aes(x=week_end_date)) +
  geom_line(aes(y=data, color=metereological_factor)) +
  facet_wrap(.~metereological_factor)+
  labs(title = "Environmental Trends Over Time", x = "Week", y = "Value")
plot_env_time_2

# 2b Distribution and Summary Statistics
# Identifying skewness in meteorological factors
# Install and load the e1071 package for calculating skewness
install.packages("e1071")
library(e1071)

# Calculate skewness
# Values between -1 and 1 suggests normality
# Values smaller than -1 or bigger than 1 suggests skewness
skewness_data <- inner_join %>%
  summarize(skew_temp = skewness(temp),
            skew_humidity = skewness(humidity),
            skew_precip = skewness(precip),
            skew_windspeed = skewness(windspeed),
            skew_dew = skewness(dew),
            skew_mpox = skewness(new_confirmed_cases)) 
skewness_data
# Temperature, humidity, windspeed and dew are normal (skewness score
# of between -1 and 1)
# Precipitation has value above 1 (2.82), indicating skewness

# New mpox confirmed cases also indicate normality. 

# Part 3: Correlation Analysis
correlation_data <- data.frame(variable = c("Temperature",
                                            "Humidity",
                                            "Windspeed",
                                            "Dew",
                                            "Precipitation"),
                               correlation_value=
                                 c(cor(inner_join$new_confirmed_cases, 
                                       inner_join$temp, method = 'pearson'),
                                   cor(inner_join$new_confirmed_cases, 
                                       inner_join$humidity, method = 'pearson'),
                                   cor(inner_join$new_confirmed_cases, 
                                       inner_join$windspeed, method = 'pearson'),
                                   cor(inner_join$new_confirmed_cases, 
                                       inner_join$dew, method = 'pearson'),
                                   cor(inner_join$new_confirmed_cases, 
                                       inner_join$precip, method = 'spearman')),
                               correlation_type = c("Pearson Correlation",
                                                    "Pearson Correlation",
                                                    "Pearson Correlation",
                                                    "Pearson Correlation",
                                                    "Spearman Correlation"))

correlation_data
# interp. Precipitation has a correlation value > 0.50, indicating 
# that precipitation may be a strong predictor. 
# Humidity and dew are moderate predictors 

# Part 4: Regression Analysis

poisson_model <- glm(new_confirmed_cases ~ temp + humidity + dew + precip + windspeed, 
                     family = poisson(link = "log"), data = inner_join)
summary(poisson_model)
poisson_model

dispersion_ratio <- sum(residuals(poisson_model, type = "pearson")^2) / poisson_model$inner_join.residual
if (dispersion_ratio > 1.5) {print("Overdispersion detected. Using Negative Binomial Model.")
  nb_model <- glm.nb(new_confirmed_cases ~ temp + humidity + dew + precip + windspeed, data = inner_join)
  summary(nb_model)}
# Step 9: Model Diagnostics
AIC(poisson_model)
BIC(poisson_model)

ggplot(data.frame(resid = residuals(poisson_model, type = "pearson")), aes(x = resid)) +
  geom_histogram(bins = 20, fill = "blue", alpha = 0.5) +
  labs(title = "Residuals Distribution")
# left skew: meaning it is a good model?
