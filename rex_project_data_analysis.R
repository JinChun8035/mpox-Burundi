library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(corrplot) 
library(MASS) # for glm function
library(pscl) 
library(arm)
library(jtools)
library(lmtest)
library(effects)

### uploading data
mpox_cases_raw_unfiltered <- read_csv('afr_cases_as_of_february_2nd.csv')
temperature_burundi_unfiltered <- read_csv('correct_metereological_data.csv')

### filtering data
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

### Inspect Data
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

### 2a Identifying time-series trends 
# Plot: Line chart (new_confirmed_cases vs. week_end_date)
plot_newcases_end_date <- inner_join %>%
  ggplot(aes(x=week_end_date, y=new_confirmed_cases)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month") +
  labs(title="Burundi New Confirmed Clade1b Mpox Cases vs Date of Data 
       Collection, weekly",
       x= "Date of Data Collection", 
       y= "Newly Confirmed Clad1b Mpox Cases") 
plot_newcases_end_date
# keep in poster -> important to show ppl that there is variations in mpox cases
# upward and downward trends 
# Drastic increase Sep-Oct, Oct-Nov, and consider why drop Dec-Jan
# Low Transmission Phases (LTP) 
# Rapid Onset Phases -> 2024-09-15 onwards suddenly above 100, drops below 100 
# 2024-12-29
# Major peak identified 2024-09-15 goes over 100, 2025-10-27 at 222 new cases
# Small peak identified 2025-01-19 where it reaches 120 (>100)
# Seems like we could cluster the data into k=2 with threshold around 125 new cases

# Plotting environmental factors over time (checking climate fluctuations)
# on one graph
# goal: are there any changes/patterns?
plot_env_time <- ggplot(inner_join, aes(x = week_end_date)) +
  geom_line(aes(y = temp, color = "Temperature")) +
  geom_line(aes(y = humidity, color = "Humidity")) +
  geom_line(aes(y = precip, color = "Precipitation")) +
  geom_line(aes(y = windspeed, color = "Wind Speed")) +
  geom_line(aes(y = dew, color = "Dew")) + 
  scale_x_date(date_labels="%b %y", date_breaks = "1 month") +
  labs(title = "Environmental Trends Over Time", x = "Week", y = "Value")
plot_env_time
# Monthly average
plot_env_monthly <- inner_join %>%
  group_by(month = lubridate::floor_date(week_end_date, 'month')) %>%
  summarize(temp = mean(temp), humidity = mean(humidity), precip = mean(precip), 
            windspeed = mean(windspeed), dew = mean(dew)) %>%
  ggplot(aes(x = month)) +
  geom_line(aes(y = temp, color = "Temperature")) +
  geom_line(aes(y = humidity, color = "Humidity")) +
  geom_line(aes(y = precip, color = "Precipitation")) +
  geom_line(aes(y = windspeed, color = "Wind Speed")) +
  geom_line(aes(y = dew, color = "Dew")) + 
  scale_x_date(date_labels="%b %y", date_breaks = "1 month") +
  labs(title = "Environmental Trends Over Time", x = "Month", y = "Value")
plot_env_monthly
# this is just for preliminary understanding: understand full data: seeing the bigger picture
# understand: is there any changes in factor over the months
# if we don't see any variations -> have to be careful making interpretations later on

# on separate graphs
plot_env_time2 <- ggplot(longer_data, aes(x=week_end_date)) +
  geom_line(aes(y=data, color=metereological_factor)) +
  facet_wrap(.~metereological_factor)+
  labs(title = "Environmental Trends Over Time", x = "Week", y = "Value")
plot_env_time2

# Mpox case bar chart + the environmental factors 
combined_data <- ggplot(inner_join, aes(x = week_end_date)) +
  geom_bar(aes(y = new_confirmed_cases), stat="identity", alpha=0.5) +
  geom_line(aes(y = temp, color = "Temperature"), size=0.8) +
  geom_line(aes(y = humidity, color = "Humidity"), size=0.8) +
  geom_line(aes(y = precip, color = "Precipitation"), size=0.8) +
  geom_line(aes(y = windspeed, color = "Wind Speed"), size=0.8) +
  geom_line(aes(y = dew, color = "Dew"), size=0.8) + 
  scale_x_date(date_labels="%b %y", date_breaks = "1 month") +
  labs(title = "Environmental Trends and New Mpox 
Cases in Burundi", 
       x = "Week", y = "Value") +
  scale_color_manual(values = c(
    "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
    "#0072B2", "#D55E00", "#CC79A7"
  ))+
  
  # Clean theme with adjustments
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(size = 28, face = "bold", hjust = 0.5),  # Bigger & centered title
    axis.title = element_text(size = 20),  # Bigger axis labels
    axis.text = element_text(size = 16),  # Bigger tick labels
    legend.title = element_text(size = 14, face = "bold"),  # Bigger legend title
    legend.text = element_text(size = 12)  # Bigger legend labels
  ) 
  
combined_data


### 2b Distribution and Summary Statistics
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

### Part 3: Correlation Analysis
correlation_data <- data.frame(variable = c("Temperature",
                                            "Humidity",
                                            "Windspeed",
                                            "Dew",
                                            "Precipitation"),
                               p_value =
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
cor.test(inner_join$new_confirmed_cases, inner_join$temp, method = "pearson")
# p value = 0.3255
cor.test(inner_join$new_confirmed_cases, inner_join$humidity, method = "pearson")
# p value = 0.06814
cor.test(inner_join$new_confirmed_cases, inner_join$windspeed, method = "pearson")
# p value = 0.8953
cor.test(inner_join$new_confirmed_cases, inner_join$dew, method = "pearson")
# p value = 0.07304
cor.test(inner_join$new_confirmed_cases, inner_join$precip, method = "kendall")
# kendall is used due to ties being present
# kendall still works with skewed data and is more robust to ties 
# p value = 0.005697

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

### Part 4: Regression Analysis

poisson_model <- glm(new_confirmed_cases ~ temp + humidity + dew + precip + windspeed, 
                     family = poisson(link = "log"), data = inner_join)
summary(poisson_model)

# OBJECTIVE: Calculate R squared values 
# Fit the full model
full_model <- glm(new_confirmed_cases ~ temp + humidity + dew + precip + windspeed, 
                  family = poisson(link = "log"), data = inner_join)
full_model
# Null model (no predictors)
null_model <- glm(new_confirmed_cases ~ 1, 
                  family = poisson(link = "log"), data = inner_join)
null_model
# Function to compute McFadden's R² 
compute_mcfadden_r2 <- function(model, null_model) {
  1 - (logLik(model) / logLik(null_model))
}
compute_mcfadden_r2
# Compute full model McFadden R²
r2_full <- compute_mcfadden_r2(full_model, null_model)

# Compute McFadden R² for models with one predictor removed
r2_temp <- compute_mcfadden_r2(update(full_model, . ~ . - temp), null_model)
r2_humidity <- compute_mcfadden_r2(update(full_model, . ~ . - humidity), null_model)
r2_dew <- compute_mcfadden_r2(update(full_model, . ~ . - dew), null_model)
r2_precip <- compute_mcfadden_r2(update(full_model, . ~ . - precip), null_model)
r2_windspeed <- compute_mcfadden_r2(update(full_model, . ~ . - windspeed), null_model)

# Print the individual pseudo R² values
r2_full # 0.296 , df= 6.  # this is the McFadden Pseudo r^2 for the full model
r2_temp # 0.188 , df = 5
r2_humidity # 0.173 , df = 5
r2_dew # 0.186, df = 5
r2_precip # 0.295, df = 5
r2_windspeed # 0.237, df = 5

### Variable selection using BIC 
stepAIC(poisson_model, direction = 'both', k = log(dim(inner_join)[1]))
# it appears that  temp + humidity + dew + windspeed are the most relevant
# predictors in our model.

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

### Poisson regression: Over-dispersion 
# check mean() and var() of dependent variable to determine if we will 
# have overdispersion in our model
mean(inner_join$new_confirmed_cases)
var(inner_join$new_confirmed_cases)
# variance is much larger than mean, suggesting that there will be 
# over-dispersion in our model

# TODO: does this mean that we use quasi-poisson regression since our data's
# dependent variable has over-dispersed count data? 

poisson_model2 <- glm(new_confirmed_cases ~ temp + humidity + dew + precip +
                         windspeed, data = inner_join, 
                       family = quasipoisson(link = "log"))
summary(poisson_model2)

# extract coefficients from first model using 'coef()'
coef1 = coef(poisson_model) 

# extract coefficients from second model
coef2 = coef(poisson_model2) 

# extract standard errors from first model using 'se.coef()'
se.coef1 = se.coef(poisson_model) 

# extract standard errors from second model
se.coef2 = se.coef(poisson_model2)

# use 'cbind()' to combine values into one dataframe
models_both <- cbind(coef1, se.coef1, coef2, se.coef2, exponent = exp(coef1)) 

# show dataframe
models_both
# shows the coefficients as same, but the SE is different

plot_summs(poisson_model, poisson_model2, scale = TRUE, exp = TRUE)


### negative binomial model
nb_model <- glm.nb(new_confirmed_cases ~ temp + humidity + dew + precip + windspeed, 
                 data = inner_join)
summary(nb_model)
# model comparison using likelihood ratio tests
lrtest(poisson_model, nb_model)
# p value is much smaller, meaning non-binary is better model

plot(residuals(nb_model, type = "deviance"))
# calculate pseudo r squared to test how well model fits 
1 - (nb_model$deviance / nb_model$null.deviance)
# gives 0.248763

### Visualising results
plot(fitted(nb_model), residuals(nb_model, type = "pearson"), 
     xlab = "Fitted Values", ylab = "Pearson Residuals", 
     main = "Residuals vs Fitted Values", pch = 16, col = "blue")
abline(h = 0, lty = 2, col = "red")
# this shows randomly scattered values, meaning this is a good model

hist(residuals(nb_model, type = "pearson"), 
     main = "Histogram of Pearson Residuals", 
     xlab = "Residuals", col = "lightblue", border = "black")
# kind of shows normality? meaning good model

plot(effect("temp", nb_model), main = "Effect of Temperature on Predicted Cases")
plot(effect("humidity", nb_model), main = "Effect of Humidity on Predicted Cases")
plot(effect("dew", nb_model), main = "Effect of Dew on Predicted Cases")
plot(effect("precip", nb_model), main = "Effect of Precipitation on Predicted Cases")
plot(effect("windspeed", nb_model), main = "Effect of Windspeed on Predicted Cases")
# suggests that precip and windspeed aren't that good of a predictors? because
# of wider spread?


### Poisson regression: Over-dispersion 
dispersion_ratio <- sum(residuals(poisson_model, type = "pearson")^2) / poisson_model$inner_join.residual 
if (dispersion_ratio > 1.5) {
  print("Overdispersion detected. Using Negative Binomial Model.")
  nb_model <- glm.nb(new_confirmed_cases ~ temp + humidity + dew + precip + windspeed, data = inner_join)
  summary(nb_model)
}

# TODO: Error in if (dispersion_ratio > 1.5) { : argument is of length zero
# Step 9: Model Diagnostics
AIC(poisson_model)
BIC(poisson_model)

poisson_plot <- ggplot(data.frame(resid = residuals(poisson_model, type = "pearson")), aes(x = resid)) +
  geom_histogram(bins = 20, fill = "blue", alpha = 0.5) +
  labs(title = "Residuals Distribution")
poisson_plot
# left skew: meaning it is a good model?


# TODO
ggsave("combined_data.png")

longer_data <- longer_data %>%
  pivot_longer(cols = c("total_confirmed_cases", "new_confirmed_cases"),
               names_to = "mpox_cases",
               values_to = "data2")



plot3 <- longer_data %>%
  ggplot(aes(x = week_end_date, y = data2, color = mpox_cases)) +
  
  # Thicker lines and bigger points for better visibility
  geom_line(size = 1.5) +  
  geom_point(size = 3) +  
  
  # Date formatting
  scale_x_date(date_labels = "%b %y", date_breaks = "1 month") +
  
  # Colorblind-friendly high-contrast palette
  scale_color_brewer(palette = "Set1") +  
  
  # Clean theme with adjustments
  theme_minimal(base_size = 16) +  
  
  # Improve readability
  labs(
    x = "Week", 
    y = "Number of Cases",
    title = "New Confirmed and Total Confirmed
    Mpox Cases Over Time in Burundi",
    color = "Case Type"  # Improves legend clarity
  ) +
  
  # Adjust font sizes and make titles bold for better visibility
  theme(
    plot.title = element_text(size = 28, face = "bold", hjust = 0.5),  # Bigger & centered title
    axis.title = element_text(size = 20),  # Bigger axis labels
    axis.text = element_text(size = 16),  # Bigger tick labels
    legend.title = element_text(size = 14, face = "bold"),  # Bigger legend title
    legend.text = element_text(size = 12)  # Bigger legend labels
  )

# Display the plot
plot3

ggsave("plot3.png", width = 12, height =8, dpi= 600)
ggsave("combined_data.png", width = 12, height =8, dpi= 600)

# rate ratio analysis using temp and humidity 
temp_hum <- inner_join %>%
    select(temp, humidity, new_confirmed_cases, week_end_date) 

median(inner_join$humidity) # 67.3871
median(inner_join$temp) # 24.62857


#calculating rate ratios
median_temp <- median(inner_join$temp, na.rm = TRUE)
temp_med_binary <- inner_join %>%
  mutate(group = ifelse(temp > median_temp, "Above Median", "Below Median"))
transmission_rates <- temp_med_binary %>%
  group_by(group) %>%
  summarise(mean_temp = mean(new_confirmed_cases, na.rm = TRUE))
transmission_rates
rate_ratio1 <- transmission_rates$mean_temp[transmission_rates$group == "Below Median"] / 
  transmission_rates$mean_temp[transmission_rates$group == "Above Median"]
rate_ratio1
# 1.21
median_dew <- median(inner_join$dew, na.rm = TRUE)
dew_med_binary <- inner_join %>%
  mutate(group = ifelse(dew > median_dew, "Above Median", "Below Median"))
transmission_rates <- dew_med_binary %>%
  group_by(group) %>%
  summarise(mean_dew = mean(new_confirmed_cases, na.rm = TRUE))
transmission_rates
rate_ratio2 <- transmission_rates$mean_dew[transmission_rates$group == "Above Median"] / 
  transmission_rates$mean_dew[transmission_rates$group == "Below Median"]
rate_ratio2
# 1.403
median_windspeed <- median(inner_join$windspeed, na.rm = TRUE)
windspeed_med_binary <- inner_join %>%
  mutate(group = ifelse(windspeed > median_windspeed, "Above Median", "Below Median"))
transmission_rates <- windspeed_med_binary %>%
  group_by(group) %>%
  summarise(mean_windspeed = mean(new_confirmed_cases, na.rm = TRUE))
transmission_rates
rate_ratio3 <- transmission_rates$mean_windspeed[transmission_rates$group == "Above Median"] / 
  transmission_rates$mean_windspeed[transmission_rates$group == "Below Median"]
rate_ratio3
# 0.99 no substantial difference
median_precip <- median(inner_join$precip, na.rm = TRUE)
precip_med_binary <- inner_join %>%
  mutate(group = ifelse(precip > median_precip, "Above Median", "Below Median"))
transmission_rates <- precip_med_binary %>%
  group_by(group) %>%
  summarise(mean_precip = mean(new_confirmed_cases, na.rm = TRUE))
transmission_rates
rate_ratio4 <- transmission_rates$mean_precip[transmission_rates$group == "Above Median"] / 
  transmission_rates$mean_precip[transmission_rates$group == "Below Median"]
rate_ratio4
# 1.67
median_humidity <- median(inner_join$humidity, na.rm = TRUE)
humidity_med_binary <- inner_join %>%
  mutate(group = ifelse(humidity > median_humidity, "Above Median", "Below Median"))
transmission_rates <- humidity_med_binary %>%
  group_by(group) %>%
  summarise(mean_humidity = mean(new_confirmed_cases, na.rm = TRUE))
transmission_rates
rate_ratio5 <- transmission_rates$mean_humidity[transmission_rates$group == "Above Median"] / 
  transmission_rates$mean_humidity[transmission_rates$group == "Below Median"]
rate_ratio5
# 1.67 

# TODO Create data frame for rate ratio vs environmental factors
rate_ratio_data <- data.frame(environmental_factor = c("Temperature", "Dew", 
                                      "Humidity", "Precipitation", "Windspeed"),
                                      rate_ratio = c(rate_ratio1, rate_ratio2, rate_ratio5, rate_ratio4, rate_ratio3))

rate_ratio_plot <- rate_ratio_data %>%
  ggplot(aes(x=environmental_factor)) +
  geom_bar(aes(y=rate_ratio), stat="identity")
rate_ratio_plot

# Seasonal impact on mpox transmissions
# TBH I feel iffy abt this cause we're only using data from when mpox first 
# arrived and up to December where there happened to be a major spike in 
# November which doesn't really correlate with dry or wet season
seasonal_impact <- inner_join %>% 
  mutate(month = month(week_end_date)) %>%
  group_by(month) %>%
  summarise(mean_new_confirmed_cases = mean(new_confirmed_cases), 
            total_new_confirmed_cases = sum(new_confirmed_cases),
            mean_humidity = mean(humidity),
            mean_precip = mean(precip))
seasonal_impact
# 2 to represent other, 1 for wet season, 0 for dry season
seasonal_impact$ID <- c(0, 0, "Dry season (Jul-Sep)", "Dry season (Jul-Sep)", 
                        "Dry season (Jul-Sep)", "Wet season (Oct-Dec)", 
                        "Wet season (Oct-Dec)", "Wet season (Oct-Dec)")
seasonal_impact <- seasonal_impact %>%
  group_by(ID) %>%
  summarise(mean_new_confirmed_cases = mean(mean_new_confirmed_cases), 
            total_new_confirmed_cases = sum(total_new_confirmed_cases))
seasonal_impact
# According to Saiful's data:	Wet Season (Oct-Dec): 161 average weekly cases 
# (2093 total cases). Dry Season (Jul-Sep): 78 average weekly cases (853 total 
# cases). Rate Ratio: 2.08× higher transmission during wet season
# WE GOT Wet Season (Oct-Dec): 161 average weekly cases 
# (2093 total cases). Dry Season (Jul-Sep): 61.5 average weekly cases (853 total 
# cases)
seasonal_impact_plot <- seasonal_impact %>%
  filter(ID != 0) %>%
  ggplot(aes(x = ID, y = mean_new_confirmed_cases)) +
  geom_bar(stat="identity") + 
  labs(title = "Seasonal Impact on Mpox Transmission", 
       x = "Season", y = "Average weekly new cases") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(size = 28, face = "bold", hjust = 0.5),  # Bigger & centered title
        axis.title = element_text(size = 20),  # Bigger axis labels
        axis.text = element_text(size = 16),  # Bigger tick labels
  ) 
seasonal_impact_plot
ggsave("seasonal_impact_plot.png", width = 12, height =8, dpi= 600)

# TODO: Optimal conditions aka envr conditions and mpox transmission
# What's our threshold? We need to explain why or how we chose it
# Choosing 65 and 2 for now based on mean month data
inner_join$ID <- ifelse(inner_join$humidity < 65 & inner_join$precip < 2, 
                        "Low humidity, low precip", inner_join$ID)
inner_join$ID <- ifelse(inner_join$humidity < 65 & inner_join$precip >= 2, 
                        "Low humidity, high precip", inner_join$ID)
inner_join$ID <- ifelse(inner_join$humidity >= 65 & inner_join$precip < 2, 
                        "High humidity, low precip", inner_join$ID)
inner_join$ID <- ifelse(inner_join$humidity >= 65 & inner_join$precip >= 2, 
                        "High humidity, high precip", inner_join$ID)
inner_join_plot <- inner_join %>%
  group_by(ID) %>%
  summarise(mean_weekly_new_cases = mean(new_confirmed_cases)) %>%
  ggplot(aes(x=ID, y=mean_weekly_new_cases)) +
  geom_bar(stat = "identity") +
  labs(title = "Environmental Conditions and Mpox Transmission",
       x = "Environmental conditions", y = "Mean weekly cases") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(size = 28, face = "bold", hjust = 0.5),  # Bigger & centered title
        axis.title = element_text(size = 20),  # Bigger axis labels
        axis.text = element_text(size = 16),  # Bigger tick labels
        legend.title = element_text(size = 14, face = "bold"),  # Bigger legend title
        legend.text = element_text(size = 12)  # Bigger legend labels
        ) 
inner_join_plot
ggsave("inner_join_plot.png", width = 12, height =8, dpi= 600)
