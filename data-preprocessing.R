library(lubridate)
library(readr)
library(tsibble)
library(ggplot2)
library(fpp3)
library(feasts)
library(tidyverse)

# read the data
data <- read.csv('database-employment.csv')

# fix dates in the original data
data$REF_DATE <- my(paste0(data$REF_DATE))

# convert to tsibble object
tdata <- data %>%
  mutate(dates = yearmonth(as.Date(data$REF_DATE))) %>% # fill in the time gaps
  as_tsibble(index = dates, key = Immigrant.status)

# View the tsibble
print(tdata)

# time plot of all immigrant status
autoplot(tdata, VALUE) +
  geom_line() + 
  labs(title = "Time plot of all immigrant status", x = "Date (2000-2023)", y = "Percentages") +
  theme_minimal()

# time plot for those born in Canada
autoplot(subset(tdata, Immigrant.status == "Born in Canada"), VALUE) +
  labs(title = "Time plot of people born in Canada", x = "Date (2000-2023)", y = "Percentages") +
  theme_minimal()

# time plot for immigrants, landed 5 or less years earlier
autoplot(subset(tdata, Immigrant.status == "Immigrants, landed 5 or less years earlier"), VALUE) +
  labs(title = "Time plot of immigrants, landed 5 or less years earlier", x = "Date (2000-2023)", y = "Percentages") +
  theme_minimal()

# time plot for immigrants, landed more than 10 years earlier
autoplot(subset(tdata, Immigrant.status == "Immigrants, landed more than 10 years earlier"), VALUE) +
  labs(title = "Time plot of immigrants, landed more than 10 years earlier", x = "Date (2000-2023)", y = "Percentages") +
  theme_minimal()

# time plot for immigrants, landed more than 5 to 10 years earlier
autoplot(subset(tdata, Immigrant.status == "Immigrants, landed more than 5 to 10 years earlier"), VALUE) +
  labs(title = "Time plot of immigrants, landed more than 5 to 10 years earlier", x = "Date (2000-2023)", y = "Percentages") +
  theme_minimal()

# time plot for landed immigrants
autoplot(subset(tdata, Immigrant.status == "Landed immigrants"), VALUE) +
  labs(title = "Time plot of landed immigrants", x = "Date (2000-2023)", y = "Percentages") +
  theme_minimal()

# time plot for total population
autoplot(subset(tdata, Immigrant.status == "Total population"), VALUE) +
  labs(title = "Time plot of the total population", x = "Date (2000-2023)", y = "Percentages") +
  theme_minimal()

# seasonal plot of all data 
tdata %>% gg_season(VALUE, labels = "both")
