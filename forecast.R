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

# autoplot function for subcategories
plot_time <- function(data, category, title, x_label = "Date (2000-2023)", y_label = "Percentages") {
  subdata = subset(data, Immigrant.status == category)
  plot <- autoplot(subdata, VALUE) +
    labs(title = title, x = x_label, y = y_label) +
    theme_minimal()
  plot2 <- subdata |> gg_lag(VALUE, geom = "point") + labs(title = category)
  plot3 <- subdata |> ACF(VALUE) |> autoplot() + labs(title = category)
  print(plot)
  print(plot2)
  print(plot3)
}
plot_time(tdata, "Born in Canada", "Time plot of people born in Canada")
plot_time(tdata, "Immigrants, landed 5 or less years earlier", "Time plot of immigrants, landed 5 or less years earlier")
plot_time(tdata, "Immigrants, landed more than 10 years earlier", "Time plot of immigrants, landed more than 10 years earlier")
plot_time(tdata, "Immigrants, landed more than 5 to 10 years earlier", "Time plot of immigrants, landed more than 5 to 10 years earlier")
plot_time(tdata, "Landed immigrants", "Time plot of landed immigrants")
plot_time(tdata, "Total population", "Time plot of the total population")

# seasonal plot of all data
tdata |> gg_season(VALUE, labels = "both")

# another seasonal plot
tdata |> gg_subseries(VALUE)

# DECOMPOSITION

# function to return decomp graph for specific category
plot_decomp <- function(data, total) {
  if (total){
    t = "all data"
  } else {
    t = unique(data$Immigrant.status)
  }
  
  decomp <- data %>% 
    model(decomp = classical_decomposition(VALUE, type = "multiplicative")) %>%
    components()
  
  print(decomp |> autoplot() + labs(title = paste("Decomposition of", t)))
  
  # seasonally adjusted data
  print(as_tsibble(decomp) |>
    autoplot(season_adjust) + labs(title = paste("Seasonally adjusted data for", t), x = "Months", y = "Percentages"))
  
  # x11 model 
  decomp_x11 <- data |>
    model(x11 = X_13ARIMA_SEATS(VALUE ~ x11())) |>
    components()
  print(decomp_x11 |> ggplot() + 
    geom_line(aes(x = dates, y = VALUE), color = "blue") +
    geom_line(aes(x = dates, y = season_adjust) , color = "red") +
    labs(title = paste("Original and seasonally asjusted data: X-11, for", t),
         caption = "Original series is shown in Blue"  ))
}

# plot all data
plot_decomp(tdata, TRUE)

# get the list of unique categories
categories <- unique(tdata$Immigrant.status)

# loop through each category and plot
plots <- lapply(categories, function(cat) {
  data_subset <- filter(tdata, Immigrant.status == cat)
  plot_decomp(data_subset, FALSE)
})


# MODEL THINGS IDK WHAT THEY ARE THOUGH

categorymodel <- function(category){
  # Set the proportion of the dataset to be used for training
  train_prop <- 0.9
  
  # Calculate the index where the training set ends
  train_end_index <- floor(train_prop * nrow(category))
  
  # Split the dataset chronologically
  train_set <- category[1:train_end_index, ]
  test_set <- category[(train_end_index + 1):nrow(category), ]
  
  # Convert to tsibble objects
  t_train <- train_set %>% 
    mutate(dates = yearmonth(as.Date(train_set$REF_DATE))) %>% 
    as_tsibble(index = dates, key = Immigrant.status)
  
  t_test <- test_set %>% 
    mutate(dates = yearmonth(as.Date(test_set$REF_DATE))) %>% 
    as_tsibble(index = dates, key = Immigrant.status)
  
  fit <- t_train |> 
    model(
      mean = MEAN(VALUE),
      naive = NAIVE(VALUE),
      snaive = SNAIVE(VALUE),
      drift = RW(VALUE ~ drift())
    )
  print(fit |>
          forecast(h = "20 months") |> accuracy(tdata))
  print(fit |>
          select(drift) |>
          gg_tsresiduals() + labs(title = category$Immigrant.status))
}

# ETS model
etsmodel <- function(category) {
  fit <- category |>
    model(ANN = ETS(VALUE ~ error("A") + trend("N") + season("N")))
  print(components(fit) |> autoplot() + labs(title = paste0("ETS model:", category$Immigrant.status)))
  print(components(fit))
  components(fit) |> left_join(fitted(fit), by = c("Immigrant.status", ".model", "dates"))
  print(fit |>
    forecast(h = "1 year") |>
    autoplot(category) +
    labs(y = "Percentages", title = paste0("Exports:", category$Immigrant.status)))
}

# regression model maybe
regression <- function(category){
  fit <- category |> 
    model(reg = TSLM(log(VALUE) ~ trend() + season()))
  print(category |> autoplot(VALUE, col = "gray") +
    geom_line(data = augment(fit), aes(y = .fitted), col = "blue") +
    labs(y = "Percentages", title = paste0("Regression model:", category$Immigrant.status)))
}
  
  

# loop through each category and train
results <- lapply(categories, function(cat) {
  data_subset <- filter(tdata, Immigrant.status == cat)
  print(data_subset)
  categorymodel(data_subset)
  etsmodel(data_subset)
  regression(data_subset)
})





