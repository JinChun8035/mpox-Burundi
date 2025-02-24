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
# TODO: have observations as Mean (Min, Max)?
burundi_cases <- mpox_cases_raw_unfiltered %>%
  filter(country == "Burundi") %>%
  subset(select = c(week_end_date:total_confirmed_cases, 
                    new_confirmed_cases)) %>%
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
# TODO: make month wise
plot_newcases_end_date <- inner_join %>%
  ggplot(aes(x=week_end_date, y=new_confirmed_cases)) +
  geom_line() +
  geom_point() +
  labs(title="Burundi New Confirmed Clade1b Mpox Cases vs Date of Data 
       Collection, weekly",
       x= "Date of Data Collection", 
       y= "Newly Confirmed Clad1b Mpox Cases")
plot_newcases_end_date
# keep in poster -> important to show ppl that there is variations in mpox cases
# upward and downward trends 
# granulation -> make month wise 
# show each individual month 
# identify which months -> peaks 
# low transmission phases -> don't know the cause -> from graph we can identify LTP
# typically needs one eyar
# decline phases -> December onwards - see general decrease in mpox cases
# rapid onset(?) phases -> August probably 
# peaks -> somewhere in November, September

# this is justification for why we are analysing metereological factors 
# why do these peaks exist?

# Major peak identified in November
# Small peaks identified in September and late January


# Plotting environmental factors over time (checking climate fluctuations)
# on one graph
# goal: are there any changes/patterns?
# TODO: month wise (monthly data)

plot_env_time <- ggplot(inner_join, aes(x = week_end_date)) +
  geom_line(aes(y = temp, color = "Temperature")) +
  geom_line(aes(y = humidity, color = "Humidity")) +
  geom_line(aes(y = precip, color = "Precipitation")) +
  geom_line(aes(y = windspeed, color = "Wind Speed")) +
  geom_line(aes(y = dew, color = "Dew")) + 
  labs(title = "Environmental Trends Over Time", x = "Week", y = "Value")
plot_env_time
# same change granulation -> show more month-wise data 
# temperature not changing much -> shows idea that it doesn't rly affect much
# july-feberuary - temp. barely fluctuates 
# need to change the axis to show fluctuation better 
# just because we don't see much fluctuation doesn't mean there is no correlation at all w mpox
# what we can show is: 

# changes: x-axis show individual months 
# averaged form of data -> monthly 
# this is just for preliminary understanding: understand full data: seeing the bigger picture
# understand: is there any changes in factor over the months
# if we don't see any variations -> have to be careful making interpretations later on
# 

# on separate graphs
plot_env_time2 <- ggplot(longer_data, aes(x=week_end_date)) +
  geom_line(aes(y=data, color=metereological_factor)) +
  facet_wrap(.~metereological_factor)+
  labs(title = "Environmental Trends Over Time", x = "Week", y = "Value")
plot_env_time2

#TODO: one graph: show mpox case bar chart + the environmental factors 
# monthly -> see example graph 

# 2b Distribution and Summary Statistics
# Identifying skewness in meteorological factors
# Install and load the e1071 package for calculating skewness
# TODO: is this sufficient or do we need histogram, boxplot, and density plots
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

cor.test(inner_join$new_confirmed_cases, inner_join$temp, method = "pearson")
cor.test(inner_join$new_confirmed_cases, inner_join$humidity, method = "pearson")
cor.test(inner_join$new_confirmed_cases, inner_join$windspeed, method = "pearson")
cor.test(inner_join$new_confirmed_cases, inner_join$dew, method = "pearson")
cor.test(inner_join$new_confirmed_cases, inner_join$precip, method = "spearman")

# look at p-value for the rule of thumb
# -0.189 for temp means negative correlation 
# goal: see if individually is correlated with mpox cases
# p value is more than 0.05 is not statistically significant 
# if its not significant - still keep it - still want to see how it acts in our regression model
# descriptive analysis: is there any variations in mpox cases at all? 
correlation_data
# the rule of thumb for pearson correlation is that +-0.3 = medium correlation
# +- 0.5 = strong corelation
# interp. Precipitation has a correlation value > 0.50, indicating 
# that precipitation may be a strong predictor. 
# Humidity and dew are moderate predictors 
# dew and temp are a weak predictors

# Part 4: Regression Analysis

poisson_model <- glm(new_confirmed_cases ~ temp + humidity + dew + precip + windspeed, 
                     family = poisson(link = "log"), data = inner_join)
summary(poisson_model)
# P values from here: only precip is not significant -> rest are ok -> can be predictors
# predictor: if it was 1 -> no effect, more than 1, associated effect, less than 1, not-associated effect
# 
# TODO: don't have R^2 value 
# coefficient level: 
# temp: 3.04 - 1 = 2.04 (-1 is the now value: if it was 1 it's no effect)
# multiply 2.04*100% = 204% -> temp has 204% times of transmitting mpox 
# whenever temp b/w the min and max temp (from part 1), there is 204% chance of transmitting mpox 
# do this for all the variables 
# when country temp is b/w x and y, experts repsonsible for trackigndisease, more resources
# and intense resources to track mpox to prevent mpox -> preparation for this during this temp range
# need r^2 value -> these predictors can only explain x% of mpox cases
# only seeing small portion of variation 
exp(coef(poisson_model))
# coefficients from here -> related risks 
dispersion_ratio <- sum(residuals(poisson_model, type = "pearson")^2) / poisson_model$inner_join.residual
if (dispersion_ratio > 1.5) {print("Overdispersion detected. Using Negative Binomial Model.")
  nb_model <- glm.nb(new_confirmed_cases ~ temp + humidity + dew + precip + windspeed, data = inner_join)
  summary(nb_model)}
# TODO: Error in if (dispersion_ratio > 1.5) { : argument is of length zero
# Step 9: Model Diagnostics
AIC(poisson_model)
BIC(poisson_model)

poissson_plot <- ggplot(data.frame(resid = residuals(poisson_model, type = "pearson")), aes(x = resid)) +
  geom_histogram(bins = 20, fill = "blue", alpha = 0.5) +
  labs(title = "Residuals Distribution")
# left skew: meaning it is a good model?

library(officer)
?read_docx()
doc <- read_docx() %>%
  body_add_par("Mpox Analysis Report", style = "heading 1") %>%
  body_add_par(paste(capture.output(summary(plot_newcases_end_date)), collapse = "\n")) %>%
  body_add_par(paste(capture.output(summary(plot_env_time)), collapse = "\n")) %>%  
  body_add_par(paste(capture.output(summary(plot_env_time2)), collapse = "\n")) %>%
  body_add_par(paste(capture.output(summary(skewness_data)), collapse = "\n")) %>%
  body_add_par(paste(capture.output(summary(skewness_data)), collapse = "\n")) %>%
  body_add_par("Demographic Analysis", style = "heading 2") %>%
  body_add_par(paste(capture.output(summary(correlation_data)), collapse = "\n")) %>%
  body_add_par("Regression Analysis", style = "heading 2") %>%
  body_add_par(paste(capture.output(summary(poissson_plot)), collapse = "\n"))

print(doc, target = "mpox_analysis_report.docx")

