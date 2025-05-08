
# 1. load packages--------------------------
library(readxl)
library(DIMORA)
library(dplyr)
library(ggplot2)
library(gridExtra)  # For arranging plots in a grid




# change directory
setwd('Github/pandemic_modelling/Data/silver/')

# 2. Load data-------------------------
## monthly data------------------
df_m<- read.csv("covid_data_monthly.csv")

# filter dataframe
df_col <- df_m %>% filter(country == "Colombia")
covid_series_col <- df_col$new_cases
# run simple bass model
bm_m <-BM(covid_series_col,display = T) # show graphical view of results / display = True
summary(bm_m)

# do for 25% of data
covid_series_col_s <- covid_series_col[1:20]
bm_m <-BM(covid_series_col_s,display = T) # show graphical view of results / display = True

## weekly data------------------------
df_w<- read.csv("covid_data_weekly.csv")

# filter dataframe
df_col_w <- df_w %>% filter(country == "Colombia")
covid_series_col_w <- df_col_w$new_cases
# run simple bass model
bm_w <-BM(covid_series_col_w,display = T) # show graphical view of results / display = True
summary(bm_w)

# do for 25% of data
covid_series_col_ws <- covid_series_col_w[1:20]
bm_w <-BM(covid_series_col_ws,display = T) # show graphical view of results / display = True



## daily data------------------------
df_d<- read.csv("covid_data.csv")

# filter dataframe
df_col_d <- df_d %>% filter(country == "Colombia")
covid_series_col_d <- df_col_d$new_cases
# run simple bass model
bm_d <-BM(covid_series_col_d,display = T) # show graphical view of results / display = True
summary(bm_d)

# do for 75 days of data
covid_series_col_ds <- covid_series_col_w[1:75]
bm_d <-BM(covid_series_col_ds,display = T) # show graphical view of results / display = True

# Extract fitted values (BM model predictions for the subset)

fitted_forecast<- predict(bm_d, newx=c(1:length(covid_series_col_d)))
fitted_forecast_inst <- make.instantaneous(fitted_forecast)

# plot actual vs fitted
plot(covid_series_col_d, type= "b")
lines(fitted_forecast_inst, lwd=2, col=2)

# 3. Bass Model -----------------------
# first implement bass model for fitting, then for forecasting

# Define function to fit Bass model and generate plot for each country
plot_bm_model <- function(country_name, df, n =150) {
  # Filter data for the specific country
  df_country <- df %>% filter(country == country_name)
  covid_series <- df_country$new_cases
  
  # Train BM model on first n days
  covid_series_train <- covid_series[1:n]
  bm_model <- BM(covid_series_train, display = FALSE)
  
  # Forecast for the entire period
  fitted_forecast <- predict(bm_model, newx = c(1:length(covid_series)))
  
  # Compute instantaneous fitted values
  fitted_forecast_inst <- make.instantaneous(fitted_forecast)
  
  # Create plot using ggplot2
  df_plot <- data.frame(
    Time = 1:length(covid_series),
    Actual = covid_series,
    Fitted = fitted_forecast_inst
  )
  
  p <- ggplot(df_plot, aes(x = Time)) +
    geom_line(aes(y = Actual), color = "black", size = 1) +
    geom_line(aes(y = Fitted), color = "red", linetype = "dashed", size = 1) +
    ggtitle(paste("COVID-19 Cases in", country_name)) +
    ylab("Daily Cases") + xlab("Time (Days)") +
    theme_minimal()
  
  return(p)
}

# Get list of unique countries
unique_countries <- unique(df_d$country)

## 3.1 Fitting-----------------------------------

### Daily-------------------

length_series <- nrow(df_col_d)

# Loop through the first 10 countries (to fit 2x5 grid)
plot_list <- lapply(unique_countries, function(country) {
  plot_bm_model(country, df_d, n=length_series)
})

# Arrange plots in a 2x5 grid
grid.arrange(grobs = plot_list, nrow = 2, ncol = 5)

### Weekly-----------------------------------


length_series <- nrow(df_col_w)
# Loop through the first 10 countries (to fit 2x5 grid)
plot_list <- lapply(unique_countries, function(country) {
  plot_bm_model(country, df_w, n=length_series)
})

# Arrange plots in a 2x5 grid
grid.arrange(grobs = plot_list, nrow = 2, ncol = 5)

### Monthly----------------------------------
length_series <- nrow(df_col)
# Loop through the first 10 countries (to fit 2x5 grid)
plot_list <- lapply(unique_countries, function(country) {
  plot_bm_model(country, df_m, n=length_series)
})

# Arrange plots in a 2x5 grid
grid.arrange(grobs = plot_list, nrow = 2, ncol = 5)




## 3.2 Forecasting--------------------------------
### Daily-------------------
# Loop through the first 10 countries (to fit 2x5 grid)
plot_list <- lapply(unique_countries, function(country) {
  plot_bm_model(country, df_d)
})

# Arrange plots in a 2x5 grid
grid.arrange(grobs = plot_list, nrow = 2, ncol = 5)

### Weekly-----------------------------------
# Loop through the first 10 countries (to fit 2x5 grid)
plot_list <- lapply(unique_countries, function(country) {
  plot_bm_model(country, df_w, n=25)
})

# Arrange plots in a 2x5 grid
grid.arrange(grobs = plot_list, nrow = 2, ncol = 5)

### Monthly----------------------------------

# Loop through the first 10 countries (to fit 2x5 grid)
plot_list <- lapply(unique_countries, function(country) {
  plot_bm_model(country, df_m, n=12)
})

# Arrange plots in a 2x5 grid
grid.arrange(grobs = plot_list, nrow = 2, ncol = 5)


