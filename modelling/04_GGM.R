# GGM script Covid

# required packages--------------------
library(readxl)
library(DIMORA)
library(dplyr)
library(ggplot2)
library(gridExtra)  # For arranging plots in a grid
library(lubridate) # for date handling in time series
library(purrr)
library(tidyr)
library(tibble)

# change directory
setwd('../Data/silver/')

# 0. Load data-------------------------
df_m<- read.csv("covid_data_monthly.csv")
df_w <- read.csv("covid_data_weekly.csv")
df_d <- read.csv("covid_data.csv")

# 1. Example GGM-----------

# mothly
df_col <- df_m %>% filter(country == "Colombia") # filter dataframe
covid_series_col <- df_col$new_cases # select cases
ggm_toy <- GGM(covid_series_col, mt='base', display = T) # run model base
summary(ggm_toy) # summary model

# weekly
df_col_w <- df_w %>% filter(country == "Colombia")
covid_series_col_w <- df_col_w$new_cases
ggm_toy_w <- GGM(covid_series_col_w, mt='base', display = T)
summary(ggm_toy_w)

# daily
df_col_d <- df_d %>% filter(country == "Colombia")
covid_series_col_d <- df_col_d$new_cases
ggm_toy_d <- GGM(covid_series_col_d, mt='base', display = T)
summary(ggm_toy_d)

# example R2 and RMSE
pred_ggm_d <- predict(ggm_toy_d, newx = c(1:length(covid_series_col_d)))
pred_ggm_d <- ts(pred_ggm_d, start = start(covid_series_col_d), frequency = frequency(covid_series_col_d))
pred.inst_ggm_d <- make.instantaneous(pred_ggm_d)
pred.inst_ggm_d <- ts(pred.inst_ggm_d, start = start(covid_series_col_d), frequency = frequency(covid_series_col_d))



cumsum_actual <- cumsum(covid_series_col_d)
cumsum_fitted <- pred_ggm_d

# Calculate R² based on cumulative values
residuals <- cumsum_actual - cumsum_fitted
ss_res <- sum(residuals^2)
ss_tot <- sum((cumsum_actual - mean(cumsum_actual))^2)
r2 <- 1 - (ss_res / ss_tot)
r2
# Calculate RMSE based on cumulative values
rmse <- sqrt(mean(residuals^2))
rmse


df_country <- df_d %>% filter(country == 'Panama')
covid_series <- df_country$new_cases
covid_series_cum <- df_country$cases

# Train GGM model on first n days
#ggm_model <- GGM(covid_series, mt='base', display = TRUE)
#summary(ggm_model)
ggm_model <- GGM(covid_series, mt=function(x) pchisq(x,10), display = TRUE, )


# Forecast for the entire period
fitted_forecast <- predict(ggm_model, newx = 1:length(covid_series))
# Check for NaN values

# Calculate cumulative sum of the actual and fitted values
cumsum_actual <- covid_series_cum
cumsum_fitted <- fitted_forecast

# Calculate R² based on cumulative values
residuals <- cumsum_actual - cumsum_fitted
ss_res <- sum(residuals^2)
ss_tot <- sum((cumsum_actual - mean(cumsum_actual))^2)
r2 <- 1 - (ss_res / ss_tot)

# Calculate RMSE based on cumulative values
rmse <- sqrt(mean(residuals^2))

rmse
r2

#2. functions-----------------------

# Define function to fit Bass model and generate plot for each country
plot_bm_model <- function(country_name, df, cumulative = FALSE) {
  # Filter data for the specific country
  df_country <- df %>% filter(country == country_name)
  covid_series <- df_country$new_cases
  
  # Train BM model on first n days
  bm_model <- BM(covid_series, display = FALSE)
  
  # Forecast for the entire period
  fitted_forecast <- predict(bm_model, newx = c(1:length(covid_series)))
  
  # Compute instantaneous fitted values
  fitted_forecast_inst <- make.instantaneous(fitted_forecast)
  
  if (cumulative == FALSE){
  # Create plot using ggplot2
    df_plot <- data.frame(
      Time = 1:length(covid_series),
      Actual = covid_series,
      Fitted = fitted_forecast_inst
    )
  }
  else{
    df_plot <- data.frame(
      Time = 1:length(covid_series),
      Actual = cumsum(covid_series),
      Fitted = fitted_forecast
    )
  }
  
  p <- ggplot(df_plot, aes(x = Time)) +
    geom_line(aes(y = Actual, color = "Actual"), size = 1) +
    geom_line(aes(y = Fitted, color = "Fitted"), linetype = "dashed", size = 1) +
    scale_color_manual(values = c("Actual" = "black", "Fitted" = "red")) +
    labs(color = "Legend") +  # Legend title
    ggtitle(paste("COVID-19 Cases in", country_name)) +
    ylab("Cases") + 
    xlab("Time") +
    theme_minimal() +
    theme(
      text = element_text(size = 8),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 8),
      plot.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8),
      plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")
    )
  return(p)
}



# run and plot GGM with catch error
plot_ggm_model <- function(country_name, df, cumulative = FALSE) {
  
  # Filter data for the specific country
  df_country <- df %>% filter(country == country_name)
  covid_series <- df_country$new_cases
  
  # Initialize the GGM model
  ggm_model <- NULL
  
  # Attempt first GGM model
  try_result <- try({
    ggm_model <- GGM(covid_series, display = FALSE)
  }, silent = TRUE)
  
  # Check if an error occurred
  if (inherits(try_result, "try-error")) {
    if (grepl("chol.default.*not positive", try_result)) {
      message("Cholesky decomposition error detected. Trying alternative model...")
      
      # Attempt alternative GGM method
      try_result_alt <- try({
        ggm_model <- GGM(covid_series, mt=function(x) pchisq(x,10), display = FALSE)
      }, silent = TRUE)
      
      # If alternative method also fails, stop execution
      if (inherits(try_result_alt, "try-error")) {
        stop("Both GGM model attempts failed. Error: ", try_result_alt)
      }
    } else {
      stop("Unexpected error in GGM model: ", try_result)
    }
  }
  
  # Ensure the model was successfully created
  if (is.null(ggm_model)) {
    stop("Failed to fit GGM model.")
  }
  
  # Forecast for the entire period
  fitted_forecast <- predict(ggm_model, newx = 1:length(covid_series))
  
  # Compute instantaneous fitted values
  fitted_forecast_inst <- make.instantaneous(fitted_forecast)
  if (cumulative == FALSE) {
  # Create plot using ggplot2
    df_plot <- data.frame(
      Time = 1:length(covid_series),
      Actual = covid_series,
      Fitted = fitted_forecast_inst
    )
  }
  else {
    df_plot <- data.frame(
      Time = 1:length(covid_series),
      Actual = cumsum(covid_series),
      Fitted = fitted_forecast
    )
  }
  p <- ggplot(df_plot, aes(x = Time)) +
    geom_line(aes(y = Actual), color = "black", size = 1) +
    geom_line(aes(y = Fitted), color = "red", linetype = "dashed", size = 1) +
    ggtitle(paste("COVID-19 Cases in", country_name)) +
    ylab("Cases") + xlab("Time") +
    theme_minimal()
  
  return(p)
}



# Function to calculate R² and RMSE for BM
calculate_metrics_bm <- function(country_name, df) {
  # Filter data for the specific country
  df_country <- df %>% filter(country == country_name)
  covid_series <- df_country$new_cases
  covid_series_cum <- df_country$cases
  
  # Train BM model on first n days
  bm_model <- BM(covid_series, display = FALSE)
  
  # Forecast for the entire period
  fitted_forecast <- predict(bm_model, newx = 1:length(covid_series))
  
  cumsum_actual <- covid_series_cum
  cumsum_fitted <- fitted_forecast
  # Calculate R² based on cumulative values
  residuals <- cumsum_actual - cumsum_fitted
  ss_res <- sum(residuals^2)
  ss_tot <- sum((cumsum_actual - mean(cumsum_actual))^2)
  r2 <- 1 - (ss_res / ss_tot)
  
  # Calculate RMSE based on cumulative values
  rmse <- sqrt(mean(residuals^2))
  
  return(c(R2 = r2, RMSE = rmse))
}


#  Function to calculate R² and RMSE for GGM based on cumulative data
calculate_metrics_ggm <- function(country_name, df) {
  # Filter data for the specific country
  df_country <- df %>% filter(country == country_name)
  covid_series <- df_country$new_cases
  covid_series_cum <- df_country$cases
  
  # Initialize GGM model variable
  ggm_model <- NULL
  
  # Attempt first GGM model
  try_result <- try({
    ggm_model <- GGM(covid_series, mt='base', display = FALSE)
  }, silent = TRUE)
  
  # Check if an error occurred
  if (inherits(try_result, "try-error")) {
    if (grepl("chol.default.*not positive", try_result)) {
      message("Cholesky decomposition error detected. Trying alternative model...")
      
      # Attempt alternative GGM method
      try_result_alt <- try({
        ggm_model <- GGM(covid_series, mt=function(x) pchisq(x,10), display = FALSE)
      }, silent = TRUE)
      
      # If alternative method also fails, stop execution
      if (inherits(try_result_alt, "try-error")) {
        stop("Both GGM model attempts failed. Error: ", try_result_alt)
      }
    } else {
      stop("Unexpected error in GGM model: ", try_result)
    }
  }
  
  # Ensure the model was successfully created
  if (is.null(ggm_model)) {
    stop("Failed to fit GGM model.")
  }
  
  # Forecast for the entire period
  fitted_forecast <- predict(ggm_model, newx = 1:length(covid_series))
  
  # Calculate cumulative sum of the actual and fitted values
  cumsum_actual <- covid_series_cum
  cumsum_fitted <- fitted_forecast
  
  # Calculate R² based on cumulative values
  residuals <- cumsum_actual - cumsum_fitted
  ss_res <- sum(residuals^2)
  ss_tot <- sum((cumsum_actual - mean(cumsum_actual))^2)
  r2 <- 1 - (ss_res / ss_tot)
  
  # Calculate RMSE based on cumulative values
  rmse <- sqrt(mean(residuals^2))
  
  return(c(R2 = r2, RMSE = rmse))
}

# try function with error catch
calculate_metrics_ggm('Panama', df_d)
plot_bm_model('Uruguay', df_d, cumulative = TRUE)

# 2. BM on COVID---------------------

# countries
unique_countries <- unique(df_d$country)
unique_countries

## 2.1 Daily-------------------

# new cases
# Loop through the first 10 countries (to fit 2x5 grid)
plot_list <- lapply(unique_countries, function(country) {
  plot_bm_model(country, df_d)
})

# Arrange plots in a 2x5 grid
grid.arrange(grobs = plot_list, nrow = 2, ncol = 5)

# cumulative cases
plot_list <- lapply(unique_countries, function(country) {
  plot_bm_model(country, df_d, cumulative =TRUE)
})

# Arrange plots in a 2x5 grid
grid.arrange(grobs = plot_list, nrow = 2, ncol = 5)



## 2.2 Weekly-----------------------------------

# Loop through the first 10 countries (to fit 2x5 grid)
plot_list <- lapply(unique_countries, function(country) {
  plot_bm_model(country, df_w, TRUE)
})



# Arrange plots in a 2x5 grid
grid.arrange(grobs = plot_list, nrow = 2, ncol = 5)

## 2.3 Monthly----------------------------------

# Loop through the first 10 countries (to fit 2x5 grid)
plot_list <- lapply(unique_countries, function(country) {
  plot_bm_model(country, df_m)
})

# Arrange plots in a 2x5 grid
grid.arrange(grobs = plot_list, nrow = 2, ncol = 5)



# 3. GGM on COVID---------------------
## 2.1 Daily-------------------

# new cases
# Loop through the first 10 countries (to fit 2x5 grid)
plot_list <- lapply(unique_countries, function(country) {
  plot_ggm_model(country, df_d)
})

# Arrange plots in a 2x5 grid
grid.arrange(grobs = plot_list, nrow = 2, ncol = 5)

# cumulative cases
# Loop through the first 10 countries (to fit 2x5 grid)
plot_list <- lapply(unique_countries, function(country) {
  plot_ggm_model(country, df_d, cumulative = TRUE)
})

# Arrange plots in a 2x5 grid
grid.arrange(grobs = plot_list, nrow = 2, ncol = 5)

## 3.2 Weekly-----------------------------------
# Loop through the first 10 countries (to fit 2x5 grid)
plot_list <- lapply(unique_countries, function(country) {
  plot_ggm_model(country, df_w, TRUE)
})

# Arrange plots in a 2x5 grid
grid.arrange(grobs = plot_list, nrow = 2, ncol = 5)

## 3.3 Monthly----------------------------------
# Loop through the first 10 countries (to fit 2x5 grid)
plot_list <- lapply(unique_countries, function(country) {
  plot_ggm_model(country, df_m)
})

# Arrange plots in a 2x5 grid
grid.arrange(grobs = plot_list, nrow = 2, ncol = 5)


# 4. Metrics BM vs GGM---------------

# Initialize empty data frames to store metrics
bm_metrics <- data.frame(Country = unique_countries, R2 = NA, RMSE = NA)
ggm_metrics <- data.frame(Country = unique_countries, R2 = NA, RMSE = NA)

## 4.1 Daily----------------

# Loop through the countries and calculate metrics for BM
bm_metrics$R2 <- sapply(unique_countries, function(country) {
  calculate_metrics_bm(country, df_d)[1]
})
bm_metrics$RMSE <- sapply(unique_countries, function(country) {
  calculate_metrics_bm(country, df_d)[2]
})

# Loop through the countries and calculate metrics for GGM
ggm_metrics$R2 <- sapply(unique_countries, function(country) {
  calculate_metrics_ggm(country, df_d)[1]
})
ggm_metrics$RMSE <- sapply(unique_countries, function(country) {
  calculate_metrics_ggm(country, df_d)[2]
})

# Combine BM and GGM metrics into one table
combined_metrics <- merge(bm_metrics, ggm_metrics, by = "Country", suffixes = c("_BM", "_GGM"))

# Display the table
print(combined_metrics)
View(combined_metrics)

## 4.2 Weekly-------------------

# Loop through the countries and calculate metrics for BM
bm_metrics$R2 <- sapply(unique_countries, function(country) {
  calculate_metrics_bm(country, df_w)[1]
})
bm_metrics$RMSE <- sapply(unique_countries, function(country) {
  calculate_metrics_bm(country, df_w)[2]
})

# Loop through the countries and calculate metrics for GGM
ggm_metrics$R2 <- sapply(unique_countries, function(country) {
  calculate_metrics_ggm(country, df_w)[1]
})
ggm_metrics$RMSE <- sapply(unique_countries, function(country) {
  calculate_metrics_ggm(country, df_w)[2]
})

# Combine BM and GGM metrics into one table
combined_metrics <- merge(bm_metrics, ggm_metrics, by = "Country", suffixes = c("_BM", "_GGM"))

# Display the table
print(combined_metrics)
View(combined_metrics)


## 4.3 Monthly-------------------

# Loop through the countries and calculate metrics for BM
bm_metrics$R2 <- sapply(unique_countries, function(country) {
  calculate_metrics_bm(country, df_m)[1]
})
bm_metrics$RMSE <- sapply(unique_countries, function(country) {
  calculate_metrics_bm(country, df_m)[2]
})

# Loop through the countries and calculate metrics for GGM
ggm_metrics$R2 <- sapply(unique_countries, function(country) {
  calculate_metrics_ggm(country, df_m)[1]
})
ggm_metrics$RMSE <- sapply(unique_countries, function(country) {
  calculate_metrics_ggm(country, df_m)[2]
})

# Combine BM and GGM metrics into one table
combined_metrics <- merge(bm_metrics, ggm_metrics, by = "Country", suffixes = c("_BM", "_GGM"))

# Display the table
print(combined_metrics)
View(combined_metrics)


# 5. GGM Residuals-------------------------

# function to get fitted values and residuals
ggm_residuals <- function(country_name, df) {
  # Filter data for the specific country
  df_country <- df %>% filter(country == country_name)
  covid_series <- df_country$new_cases
  covid_series_cum <- df_country$cases
  
  # Initialize GGM model variable
  ggm_model <- NULL
  
  # Attempt first GGM model
  try_result <- try({
    ggm_model <- GGM(covid_series, mt='base', display = FALSE)
  }, silent = TRUE)
  
  # Check if an error occurred
  if (inherits(try_result, "try-error")) {
    if (grepl("chol.default.*not positive", try_result)) {
      message("Cholesky decomposition error detected. Trying alternative model...")
      
      # Attempt alternative GGM method
      try_result_alt <- try({
        ggm_model <- GGM(covid_series, mt=function(x) pchisq(x,10), display = FALSE)
      }, silent = TRUE)
      
      # If alternative method also fails, stop execution
      if (inherits(try_result_alt, "try-error")) {
        stop("Both GGM model attempts failed. Error: ", try_result_alt)
      }
    } else {
      stop("Unexpected error in GGM model: ", try_result)
    }
  }
  
  # Ensure the model was successfully created
  if (is.null(ggm_model)) {
    stop("Failed to fit GGM model.")
  }
  
  # Forecast for the entire period
  fitted_forecast <- predict(ggm_model, newx = 1:length(covid_series))
  
  # Calculate residuals
  residuals <- covid_series_cum - fitted_forecast
  
  
  
  # Create and return a dataframe with fitted values and residuals
  df_residuals <- data.frame(
    Time = 1:length(covid_series),
    Fitted = fitted_forecast,
    Residuals = residuals,
    Cases = covid_series_cum # actual covid cases
    
  )
  
  return(df_residuals)
}

# try it out
output <- ggm_residuals('Chile', df_m)
tsdisplay(output$Residuals)

## 5.1 Daily-----------
library(patchwork)

# Create a list of ggplot objects (if applicable)
plot_list <- lapply(unique_countries, function(country) {
  print(country)
  df_res <- ggm_residuals(country, df_d)
  residual_ts <- ts(df_res$Residuals)
  p <- autoplot(residual_ts) + ggtitle(paste("Residuals -", country))
  return(p)
})

# Combine plots using patchwork
combined_plot <- wrap_plots(plot_list, nrow = 2, ncol = 5)
print(combined_plot)

## 5.2 Weekly-----------

# Create a list of ggplot objects (if applicable)
plot_list <- lapply(unique_countries, function(country) {
  print(country)
  df_res <- ggm_residuals(country, df_w)
  residual_ts <- ts(df_res$Residuals)
  p <- autoplot(residual_ts) + ggtitle(paste("Residuals -", country))
  return(p)
})

# Combine plots using patchwork
combined_plot <- wrap_plots(plot_list, nrow = 2, ncol = 5)
print(combined_plot)

## 5.3 Monthly-----------
# Create a list of ggplot objects (if applicable)
plot_list <- lapply(unique_countries, function(country) {
  print(country)
  df_res <- ggm_residuals(country, df_m)
  residual_ts <- ts(df_res$Residuals)
  p <- autoplot(residual_ts) + ggtitle(paste("Residuals -", country))
  return(p)
})

# Combine plots using patchwork
combined_plot <- wrap_plots(plot_list, nrow = 2, ncol = 5)
print(combined_plot)


# 6. GGM Refinement--------------------

# Function to process each country
process_country <- function(country, df, save_dir = "plots") {
  # Step 1: Get GGM residuals, fitted values, and actual cases
  df_ggm <- ggm_residuals(country, df)
  
  # Step 2: Fit auto.arima using GGM fitted values as xreg
  arima_model <- auto.arima(df_ggm$Cases, xreg = df_ggm$Fitted)
  
  # Step 3: Get fitted values from the ARIMA model
  arima_fitted <- fitted(arima_model)
  
  # Step 4: Create a data frame for plotting
  plot_data <- data.frame(
    Time = df_ggm$Time,
    Actual = df_ggm$Cases,
    Fitted = arima_fitted
  )
  
  # Step 5: Plot actual vs. fitted values
  plot <- ggplot(plot_data, aes(x = Time)) +
    geom_line(aes(y = Actual, color = "Actual"), linewidth = 1) +
    geom_line(aes(y = Fitted, color = "Fitted"), linewidth = 1, linetype = "dashed") +
    labs(
      title = paste(country),
      x = "Time",
      y = "Cases",
      color = "Legend"
    ) +
    theme_minimal() +
    scale_color_manual(values = c("Actual" = "blue", "Fitted" = "red")) +
    theme(
      legend.position = c(0.9, 0.1), #Bottom right
    )
  
  return(plot)
}

## 6.1 Daily-------------------
# Iterate over each country and process
plot_list <- lapply(unique_countries, function(country) {
  process_country(country, df_d)
})

# Arrange plots in a 2x5 grid
grid.arrange(grobs = plot_list, nrow = 2, ncol = 5)

## 6.2 Weekly-------------------
# Iterate over each country and process
plot_list <- lapply(unique_countries, function(country) {
  process_country(country, df_w)
})

# Arrange plots in a 2x5 grid
grid.arrange(grobs = plot_list, nrow = 2, ncol = 5)


## 6.3 Monthly-------------------
# Iterate over each country and process
plot_list <- lapply(unique_countries, function(country) {
  process_country(country, df_m)
})

# Arrange plots in a 2x5 grid
grid.arrange(grobs = plot_list, nrow = 2, ncol = 5)


# 7. GGM Refinement Metrics----------------

# Function to compute R² and RMSE for each country
process_country_metrics <- function(country, df) {
  # Step 1: Get GGM residuals, fitted values, and actual cases
  df_ggm <- ggm_residuals(country, df)
  
  # Step 2: Fit auto.arima using GGM fitted values as xreg
  arima_model <- auto.arima(df_ggm$Cases, xreg = df_ggm$Fitted)
  
  # Step 3: Get fitted values from the ARIMA model
  arima_fitted <- fitted(arima_model)
  
  # Step 4: Compute R² and RMSE
  actual <- df_ggm$Cases
  fitted <- arima_fitted
  
  # R² calculation
  ss_total <- sum((actual - mean(actual))^2)
  ss_residual <- sum((actual - fitted)^2)
  r_squared <- 1 - (ss_residual / ss_total)
  
  # RMSE calculation
  rmse <- sqrt(mean((actual - fitted)^2))
  
  # Step 5: Return results as a data frame
  result <- data.frame(
    Country = country,
    R2 = r_squared,
    RMSE = rmse
  )
  
  return(result)
}

## 7.1 Daily------------

metrics_df <- lapply(unique_countries, function(country) {
  process_country_metrics(country, df_d)
}) %>% bind_rows()

# Print the resulting data frame
print(metrics_df)
View(metrics_df)

## 7.2 Weekly------------

metrics_df <- lapply(unique_countries, function(country) {
  process_country_metrics(country, df_w)
}) %>% bind_rows()

# Print the resulting data frame

View(metrics_df)


## 7.3 Monthly------------

metrics_df <- lapply(unique_countries, function(country) {
  process_country_metrics(country, df_m)
}) %>% bind_rows()

# Print the resulting data frame
View(metrics_df)

# Part 2: Epidemics********************************--------------------------

# 8. Load data
df_dengue <- read.csv("dengue_no_split.csv")
df_zika<- read.csv("zika.csv")
df_chic <- read.csv("chicunguya.csv")
df_var <-read.csv("varicela.csv")
head(df_dengue)
plot(df_dengue$Casos)
plot(df_zika$Casos)
plot(df_chic$Casos)
plot(df_var$Casos)

# 9. Adjust format--------------------

## 9.1 Time series objects------------

### 9.1.1 Dengue-------------------

# Ensure the DATE column is in Date format
df_dengue$DATE <- as.Date(df_dengue$DATE)

# Create a time series object
ts_casos_dengue <- ts(
  df_dengue$Casos, 
  frequency = 52, 
  start = c(
    year(min(df_dengue$DATE)), 
    week(min(df_dengue$DATE))
  )
)
plot(ts_casos_dengue, main = "Weekly Dengue Cases", xlab = "Time", ylab = "Casos")
plot(diff(ts_casos_dengue), main = "Weekly Dengue Cases-Diff", xlab = "Time", ylab = "Casos")

### 9.1.2 Zika-------------------

# Ensure the DATE column is in Date format
df_zika$DATE <- as.Date(df_zika$DATE)

# Create a time series object
ts_casos_zika <- ts(
  df_zika$Casos, 
  frequency = 52, 
  start = c(
    year(min(df_zika$DATE)), 
    week(min(df_zika$DATE))
  )
)
plot(ts_casos_zika, main = "Weekly Zika Cases", xlab = "Time", ylab = "Casos")
plot(diff(ts_casos_zika), main = "Weekly Zika-Diff", xlab = "Time", ylab = "Casos")

### 9.1.3 Chicunguya-------------------

# Ensure the DATE column is in Date format
df_chic$DATE <- as.Date(df_chic$DATE)

# Create a time series object
ts_casos_chic <- ts(
  df_chic$Casos, 
  frequency = 52, 
  start = c(
    year(min(df_chic$DATE)), 
    week(min(df_chic$DATE))
  )
)
plot(ts_casos_chic, main = "Weekly Chicunguya Cases", xlab = "Time", ylab = "Casos")
plot(diff(ts_casos_chic), main = "Weekly Chicunguya Zika-Diff", xlab = "Time", ylab = "Casos")


### 9.1.4 Varicela-------------------

# Ensure the DATE column is in Date format
df_var$DATE <- as.Date(df_var$DATE)

# Create a time series object
ts_casos_var <- ts(
  df_var$Casos, 
  frequency = 52, 
  start = c(
    year(min(df_var$DATE)), 
    week(min(df_var$DATE))
  )
)
plot(ts_casos_var, main = "Weekly Varicela Cases", xlab = "Time", ylab = "Casos")
plot(diff(ts_casos_chic), main = "Weekly Varicela-Diff", xlab = "Time", ylab = "Casos")



# 10. adjust functions----------------------
plot_bm_model_epidemics <- function( time_series, cumulative = FALSE) {

  # Train BM model on first n days
  bm_model <- BM(time_series, display = FALSE)
  
  # Forecast for the entire period
  fitted_forecast <- predict(bm_model, newx = c(1:length(time_series)))
  
  # Compute instantaneous fitted values
  fitted_forecast_inst <- make.instantaneous(fitted_forecast)
  
  if (cumulative == FALSE){
    # Create plot using ggplot2
    df_plot <- data.frame(
      Time = 1:length(time_series),
      Actual = time_series,
      Fitted = fitted_forecast_inst
    )
  }
  else{
    df_plot <- data.frame(
      Time = 1:length(time_series),
      Actual = cumsum(time_series),
      Fitted = fitted_forecast
    )
  }
  
  p <- ggplot(df_plot, aes(x = Time)) +
    geom_line(aes(y = Actual, color = "Actual"), size = 1) +
    geom_line(aes(y = Fitted, color = "Fitted"), linetype = "dashed", size = 1) +
    scale_color_manual(values = c("Actual" = "black", "Fitted" = "red")) +
    labs(color = "Legend") +  # Legend title
    ggtitle(paste("Weekly Cases")) +
    ylab("Cases") + 
    xlab("Time") +
    theme_minimal()
  print(p)
}


# run and plot GGM with catch error
plot_ggm_model_epidemics <- function(time_series, cumulative = FALSE) {
  
  # Initialize the GGM model
  ggm_model <- NULL
  
  # Attempt first GGM model
  try_result <- try({
    ggm_model <- GGM(time_series, display = FALSE)
  }, silent = TRUE)
  
  # Check if an error occurred
  if (inherits(try_result, "try-error")) {
    if (grepl("chol.default.*not positive", try_result)) {
      message("Cholesky decomposition error detected. Trying alternative model...")
      
      # Attempt alternative GGM method
      try_result_alt <- try({
        ggm_model <- GGM(time_series, mt=function(x) pchisq(x,10), display = FALSE)
      }, silent = TRUE)
      
      # If alternative method also fails, stop execution
      if (inherits(try_result_alt, "try-error")) {
        stop("Both GGM model attempts failed. Error: ", try_result_alt)
      }
    } else {
      stop("Unexpected error in GGM model: ", try_result)
    }
  }
  
  # Ensure the model was successfully created
  if (is.null(ggm_model)) {
    stop("Failed to fit GGM model.")
  }
  
  # Forecast for the entire period
  fitted_forecast <- predict(ggm_model, newx = 1:length(time_series))
  
  # Compute instantaneous fitted values
  fitted_forecast_inst <- make.instantaneous(fitted_forecast)
  if (cumulative == FALSE) {
    # Create plot using ggplot2
    df_plot <- data.frame(
      Time = 1:length(time_series),
      Actual = time_series,
      Fitted = fitted_forecast_inst
    )
  }
  else {
    df_plot <- data.frame(
      Time = 1:length(time_series),
      Actual = cumsum(time_series),
      Fitted = fitted_forecast
    )
  }
  p <- ggplot(df_plot, aes(x = Time)) +
    geom_line(aes(y = Actual), color = "black", size = 1) +
    geom_line(aes(y = Fitted), color = "red", linetype = "dashed", size = 1) +
    ggtitle(paste("Weekly Cases")) +
    ylab("Cases") + xlab("Time") +
    theme_minimal()
  print(p)
}



# Function to calculate R² and RMSE for BM
calculate_metrics_bm_epidemics <- function(time_series) {
 
  time_series_cum <- cumsum(time_series)
  
  # Train BM model on first n days
  bm_model <- BM(time_series, display = FALSE)
  
  # Forecast for the entire period
  fitted_forecast <- predict(bm_model, newx = 1:length(time_series))
  
  cumsum_actual <- time_series_cum
  cumsum_fitted <- fitted_forecast
  # Calculate R² based on cumulative values
  residuals <- cumsum_actual - cumsum_fitted
  ss_res <- sum(residuals^2)
  ss_tot <- sum((cumsum_actual - mean(cumsum_actual))^2)
  r2 <- 1 - (ss_res / ss_tot)
  
  # Calculate RMSE based on cumulative values
  rmse <- sqrt(mean(residuals^2))
  
  return(c(R2 = r2, RMSE = rmse))
}


#  Function to calculate R² and RMSE for GGM based on cumulative data
calculate_metrics_ggm_epidemics <- function(time_series) {

  time_series_cum <- cumsum(time_series)  
  # Initialize GGM model variable
  ggm_model <- NULL
  
  # Attempt first GGM model
  try_result <- try({
    ggm_model <- GGM(time_series, mt='base', display = FALSE)
  }, silent = TRUE)
  
  # Check if an error occurred
  if (inherits(try_result, "try-error")) {
    if (grepl("chol.default.*not positive", try_result)) {
      message("Cholesky decomposition error detected. Trying alternative model...")
      
      # Attempt alternative GGM method
      try_result_alt <- try({
        ggm_model <- GGM(time_series, mt=function(x) pchisq(x,10), display = FALSE)
      }, silent = TRUE)
      
      # If alternative method also fails, stop execution
      if (inherits(try_result_alt, "try-error")) {
        stop("Both GGM model attempts failed. Error: ", try_result_alt)
      }
    } else {
      stop("Unexpected error in GGM model: ", try_result)
    }
  }
  
  # Ensure the model was successfully created
  if (is.null(ggm_model)) {
    stop("Failed to fit GGM model.")
  }
  
  # Forecast for the entire period
  fitted_forecast <- predict(ggm_model, newx = 1:length(time_series))
  
  # Calculate cumulative sum of the actual and fitted values
  cumsum_actual <- time_series_cum
  cumsum_fitted <- fitted_forecast
  
  # Calculate R² based on cumulative values
  residuals <- cumsum_actual - cumsum_fitted
  ss_res <- sum(residuals^2)
  ss_tot <- sum((cumsum_actual - mean(cumsum_actual))^2)
  r2 <- 1 - (ss_res / ss_tot)
  
  # Calculate RMSE based on cumulative values
  rmse <- sqrt(mean(residuals^2))
  
  return(c(R2 = r2, RMSE = rmse))
}

ggm_residuals_epidemics <- function(time_series) {
  
  
  time_series_cum <- cumsum(time_series)
  
  # Initialize GGM model variable
  ggm_model <- NULL
  
  # Attempt first GGM model
  try_result <- try({
    ggm_model <- GGM(time_series, mt='base', display = FALSE)
  }, silent = TRUE)
  
  # Check if an error occurred
  if (inherits(try_result, "try-error")) {
    if (grepl("chol.default.*not positive", try_result)) {
      message("Cholesky decomposition error detected. Trying alternative model...")
      
      # Attempt alternative GGM method
      try_result_alt <- try({
        ggm_model <- GGM(time_series, mt=function(x) pchisq(x,10), display = FALSE)
      }, silent = TRUE)
      
      # If alternative method also fails, stop execution
      if (inherits(try_result_alt, "try-error")) {
        stop("Both GGM model attempts failed. Error: ", try_result_alt)
      }
    } else {
      stop("Unexpected error in GGM model: ", try_result)
    }
  }
  
  # Ensure the model was successfully created
  if (is.null(ggm_model)) {
    stop("Failed to fit GGM model.")
  }
  
  # Forecast for the entire period
  fitted_forecast <- predict(ggm_model, newx = 1:length(time_series))
  
  # Calculate residuals
  residuals <- time_series_cum - fitted_forecast
  
  
  
  # Create and return a dataframe with fitted values and residuals
  df_residuals <- data.frame(
    Time = 1:length(time_series),
    Fitted = fitted_forecast,
    Residuals = residuals,
    Cases = time_series_cum # actual covid cases
    
  )
  
  return(df_residuals)
}


process_epidemic <- function(time_series, epidemic) {
  # Step 1: Get GGM residuals, fitted values, and actual cases
  df_ggm <- ggm_residuals_epidemics(time_series)
  
  # Step 2: Fit auto.arima using GGM fitted values as xreg
  arima_model <- auto.arima(df_ggm$Cases, xreg = df_ggm$Fitted)
  
  # Step 3: Get fitted values from the ARIMA model
  arima_fitted <- fitted(arima_model)
  
  # Step 4: Create a data frame for plotting
  plot_data <- data.frame(
    Time = df_ggm$Time,
    Actual = df_ggm$Cases,
    Fitted = arima_fitted
  )
  
  # Step 5: Plot actual vs. fitted values
  plot <- ggplot(plot_data, aes(x = Time)) +
    geom_line(aes(y = Actual, color = "Actual"), linewidth = 1) +
    geom_line(aes(y = Fitted, color = "Fitted"), linewidth = 1, linetype = "dashed") +
    labs(
      title = epidemic,
      x = "Time",
      y = "Cases",
      color = "Legend"
    ) +
    theme_minimal() +
    scale_color_manual(values = c("Actual" = "blue", "Fitted" = "red")) +
    theme(
      legend.position = c(0.9, 0.1), #Bottom right
    )
  
  return(plot)
}



process_epidemic_metrics <- function(time_series) {
  # Step 1: Get GGM residuals, fitted values, and actual cases
  df_ggm <- ggm_residuals_epidemics(time_series)
  
  # Step 2: Fit auto.arima using GGM fitted values as xreg
  arima_model <- auto.arima(df_ggm$Cases, xreg = df_ggm$Fitted)
  
  # Step 3: Get fitted values from the ARIMA model
  arima_fitted <- fitted(arima_model)
  
  # Step 4: Compute R² and RMSE
  actual <- df_ggm$Cases
  fitted <- arima_fitted
  
  # R² calculation
  ss_total <- sum((actual - mean(actual))^2)
  ss_residual <- sum((actual - fitted)^2)
  r_squared <- 1 - (ss_residual / ss_total)
  
  # RMSE calculation
  rmse <- sqrt(mean((actual - fitted)^2))
  
  # Step 5: Return results as a data frame
  result <- data.frame(
    R2 = r_squared,
    RMSE = rmse
  )
  
  return(result)
}


# 11. BM Epidemics-------------------------

## 11.1 Dengue-------------

# new cases
plot_bm_model_epidemics(ts_casos_dengue, cumulative = FALSE)
# cumulative
plot_bm_model_epidemics(ts_casos_dengue, cumulative = TRUE)

## 11.2 Zika-------------
# new cases
plot_bm_model_epidemics(ts_casos_zika, cumulative = FALSE)
# cumulative
plot_bm_model_epidemics(ts_casos_zika, cumulative = TRUE)

## 11.3 Chicunguya-------------
# new cases
plot_bm_model_epidemics(ts_casos_chic, cumulative = FALSE)
# cumulative
plot_bm_model_epidemics(ts_casos_chic, cumulative = TRUE)

## 11.4 Varicela-------------
# new cases
plot_bm_model_epidemics(ts_casos_var, cumulative = FALSE)
# cumulative
plot_bm_model_epidemics(ts_casos_var, cumulative = TRUE)


# 12. GGM Epidemics----------------------

## 12.1 Dengue-------------

# new cases
plot_ggm_model_epidemics(ts_casos_dengue, cumulative = FALSE)
# cumulative
plot_ggm_model_epidemics(ts_casos_dengue, cumulative = TRUE)

## 12.2 Zika-------------
# new cases
plot_ggm_model_epidemics(ts_casos_zika, cumulative = FALSE)
# cumulative
plot_ggm_model_epidemics(ts_casos_zika, cumulative = TRUE)

## 12.3 Chikungunya-------------
# new cases
plot_ggm_model_epidemics(ts_casos_chic, cumulative = FALSE)
# cumulative
plot_ggm_model_epidemics(ts_casos_chic, cumulative = TRUE)

## 12.4 Varicela-------------
# new cases
plot_ggm_model_epidemics(ts_casos_var, cumulative = FALSE)
# cumulative
plot_ggm_model_epidemics(ts_casos_var, cumulative = TRUE)



# 13. BM vs GGM Metrics Epidemics---------------

# Assuming ts_casos_dengue, ts_casos_zika, and ts_casos_chic are your time series
series_list <- list(
  Dengue = ts_casos_dengue,
  Zika = ts_casos_zika,
  Chicungunya = ts_casos_chic,
  Varicela = ts_casos_var
)

# Initialize a data frame to store the results
results <- data.frame(
  Series = character(),
  Model = character(),
  R2 = numeric(),
  RMSE = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each series and calculate metrics for both models
for (name in names(series_list)) {
  time_series <- series_list[[name]]
  
  # Calculate metrics for BM model
  bm_metrics <- calculate_metrics_bm_epidemics(time_series)
  results <- rbind(results, data.frame(
    Series = name,
    Model = "BM",
    R2 = bm_metrics["R2"],
    RMSE = bm_metrics["RMSE"],
    stringsAsFactors = FALSE
  ))
  
  # Calculate metrics for GGM model
  ggm_metrics <- calculate_metrics_ggm_epidemics(time_series)
  results <- rbind(results, data.frame(
    Series = name,
    Model = "GGM",
    R2 = ggm_metrics["R2"],
    RMSE = ggm_metrics["RMSE"],
    stringsAsFactors = FALSE
  ))
}

# Print the results table
print(results)
View(results)

# 14. GGM Epidemics residuals---------------------

## 14.1 Dengue----------------
df_res <- ggm_residuals_epidemics(ts_casos_dengue)
residual_ts <- ts(df_res$Residuals)
autoplot(residual_ts) + ggtitle("Residuals Dengue")

## 14.2 Zika----------------
df_res <- ggm_residuals_epidemics(ts_casos_zika)
residual_ts <- ts(df_res$Residuals)
autoplot(residual_ts) + ggtitle("Residuals Zika")

## 14.3 Chicunguya----------------
df_res <- ggm_residuals_epidemics(ts_casos_chic)
residual_ts <- ts(df_res$Residuals)
autoplot(residual_ts) + ggtitle("Residuals Chicunguya")

## 14.3 Varicela----------------
df_res <- ggm_residuals_epidemics(ts_casos_var)
residual_ts <- ts(df_res$Residuals)
autoplot(residual_ts) + ggtitle("Residuals Varicela")


# they all have structure

# 15. GGM + Refinement Epidemics---------------

## 15.1 Dengue---------------
process_epidemic(ts_casos_dengue, 'Dengue')

## 15.2 Zika---------------
process_epidemic(ts_casos_zika, 'Zika')

## 15.3 Chicunguya---------------
process_epidemic(ts_casos_chic, 'Chikungunya')

## 15.4 Varicela---------------
process_epidemic(ts_casos_var, 'Chickenpox')


# 16. GGM + Refinement Metrics
# Initialize a data frame to store the results
results_arima <- data.frame(
  Series = character(),
  R2 = numeric(),
  RMSE = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each series and calculate metrics using process_epidemic_metrics
for (name in names(series_list)) {
  time_series <- series_list[[name]]
  
  # Calculate metrics using process_epidemic_metrics
  metrics <- process_epidemic_metrics(time_series)
  
  # Append the results to the data frame
  results_arima <- rbind(results_arima, data.frame(
    Series = name,
    R2 = metrics["R2"],
    RMSE = metrics["RMSE"],
    stringsAsFactors = FALSE
  ))
}

# Print the results table
View(results_arima)

# Split Data********************************------------------------------------
# 16. New functions-----------------------------
plot_bm_model_test <- function(country_name, df, test_split = 0.2, cumulative = FALSE) {
  # Filter data for the specific country
  df_country <- df %>% filter(country == country_name)
  covid_series <- df_country$new_cases
  
  # Define train-test split index
  split_index <- floor((1 - test_split) * length(covid_series))
  
  # Split data into training and test sets
  train_series <- covid_series[1:split_index]
  test_series <- covid_series[(split_index + 1):length(covid_series)]
  
  # Train BM model on training set
  bm_model <- BM(train_series, display = FALSE)
  
  # Forecast for the entire period
  full_forecast <- predict(bm_model, newx = 1:length(covid_series))
  
  # Compute instantaneous fitted values
  full_forecast_inst <- make.instantaneous(full_forecast)
  
  # Prepare data for plotting (show only test set forecast)
  if (cumulative == FALSE) {
    df_plot <- data.frame(
      Time = 1:length(covid_series),
      Actual = covid_series,
      Fitted = c(rep(NA, split_index), full_forecast_inst[(split_index + 1):length(covid_series)])  # Only show test forecast
    )
  } else {
    df_plot <- data.frame(
      Time = 1:length(covid_series),
      Actual = cumsum(covid_series),
      Fitted = c(rep(NA, split_index), full_forecast[(split_index + 1):length(covid_series)])  # Only show test forecast
    )
  }
  
  # Create plot
  p <- ggplot(df_plot, aes(x = Time)) +
    geom_line(aes(y = Actual, color = "Actual"), size = 1) +
    geom_line(aes(y = Fitted, color = "Fitted"), linetype = "dashed", size = 1) +
    scale_color_manual(values = c("Actual" = "black", "Fitted" = "red")) +
    labs(color = "Legend") +  # Legend title
    ggtitle(paste("COVID-19 Cases in", country_name)) +
    ylab("Cases") + 
    xlab("Time") +
    theme_minimal() +
    theme(
      text = element_text(size = 8),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 8),
      plot.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8),
      plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")
    )
  
  return(p)
}


save_bm_model_test <- function(country_name, df, test_split = 0.2, cumulative = FALSE) {
  # Filter data for the specific country
  df_country <- df %>% filter(country == country_name)
  covid_series <- df_country$new_cases
  
  # Define train-test split index
  split_index <- floor((1 - test_split) * length(covid_series))
  
  # Train BM model on training set
  bm_model <- BM(covid_series[1:split_index], display = FALSE)
  
  # Forecast for the entire period
  full_forecast <- predict(bm_model, newx = 1:length(covid_series))
  
  # Prepare data for plotting
  if (cumulative == FALSE) {
    df_plot <- data.frame(
      Time = df_country$date,
      Actual = covid_series,
      Fitted = make.instantaneous(full_forecast)  # Full instantaneous forecast
    )
  } else {
    df_plot <- data.frame(
      Time = df_country$date,
      Actual = cumsum(covid_series),
      Fitted = full_forecast  # Full cumulative forecast
    )
  }
  
  # Add train/test indicator
  df_plot$Period <- ifelse(seq_along(covid_series) <= split_index, "Train", "Test")
  
  return(df_plot)
}





plot_ggm_model_test <- function(country_name, df, test_split = 0.2, cumulative = FALSE) {
  
  # Filter data for the specific country
  df_country <- df %>% filter(country == country_name)
  covid_series <- df_country$new_cases
  
  # Define train-test split index
  split_index <- floor((1 - test_split) * length(covid_series))
  
  # Split data into training and test sets
  train_series <- covid_series[1:split_index]
  test_series <- covid_series[(split_index + 1):length(covid_series)]
  
  # Initialize the GGM model
  ggm_model <- NULL
  
  # Attempt first GGM model
  try_result <- try({
    ggm_model <- GGM(train_series, display = FALSE)
  }, silent = TRUE)
  
  # Check if an error occurred
  if (inherits(try_result, "try-error")) {
    if (grepl("chol.default.*not positive", try_result)) {
      message("Cholesky decomposition error detected. Trying alternative model...")
      
      # Attempt alternative GGM method
      try_result_alt <- try({
        ggm_model <- GGM(train_series, mt=function(x) pchisq(x,10), display = FALSE)
      }, silent = TRUE)
      
      # If alternative method also fails, stop execution
      if (inherits(try_result_alt, "try-error")) {
        stop("Both GGM model attempts failed. Error: ", try_result_alt)
      }
    } else {
      stop("Unexpected error in GGM model: ", try_result)
    }
  }
  
  # Ensure the model was successfully created
  if (is.null(ggm_model)) {
    stop("Failed to fit GGM model.")
  }
  
  # Forecast for the entire period
  full_forecast <- predict(ggm_model, newx = 1:length(covid_series))
  
  # Compute instantaneous fitted values
  full_forecast_inst <- make.instantaneous(full_forecast)
  
  # Prepare data for plotting (only test set forecast is plotted)
  if (cumulative == FALSE) {
    df_plot <- data.frame(
      Time = 1:length(covid_series),
      Actual = covid_series,
      Fitted = c(rep(NA, split_index), full_forecast_inst[(split_index + 1):length(covid_series)]) # Only show test forecast
    )
  } else {
    df_plot <- data.frame(
      Time = 1:length(covid_series),
      Actual = cumsum(covid_series),
      Fitted = c(rep(NA, split_index), full_forecast[(split_index + 1):length(covid_series)]) # Only show test forecast
    )
  }
  
  # Create plot
  p <- ggplot(df_plot, aes(x = Time)) +
    geom_line(aes(y = Actual), color = "black", size = 1) +
    geom_line(aes(y = Fitted), color = "red", linetype = "dashed", size = 1) +
    ggtitle(paste("COVID-19 Cases in", country_name)) +
    ylab("Cases") + 
    xlab("Time") +
    theme_minimal()+
    theme(plot.title = element_text(size = 12))
  
  return(p)
}

save_ggm_model_test <- function(country_name, df, test_split = 0.2, cumulative = FALSE) {
  
  # Filter data for the specific country
  df_country <- df %>% filter(country == country_name)
  covid_series <- df_country$new_cases
  
  # Define train-test split index
  split_index <- floor((1 - test_split) * length(covid_series))
  
  # Train GGM model on training set
  ggm_model <- NULL
  try_result <- try({
    ggm_model <- GGM(covid_series[1:split_index], display = FALSE)
  }, silent = TRUE)
  
  # If first attempt fails, try alternative method
  if (inherits(try_result, "try-error")) {
    if (grepl("chol.default.*not positive", try_result)) {
      try_result_alt <- try({
        ggm_model <- GGM(covid_series[1:split_index], mt=function(x) pchisq(x,10), display = FALSE)
      }, silent = TRUE)
      if (inherits(try_result_alt, "try-error")) {
        stop("Both GGM model attempts failed for ", country_name)
      }
    } else {
      stop("Unexpected error for ", country_name, ": ", try_result)
    }
  }
  
  # Forecast for the entire period
  full_forecast <- predict(ggm_model, newx = 1:length(covid_series))
  
  # Prepare data for plotting
  if (cumulative == FALSE) {
    df_plot <- data.frame(
      Time = df_country$date,
      Actual = covid_series,
      Fitted = make.instantaneous(full_forecast),  # Full instantaneous forecast
      Country = country_name
    )
  } else {
    df_plot <- data.frame(
      Time = df_country$date,
      Actual = cumsum(covid_series),
      Fitted = full_forecast,  # Full cumulative forecast
      Country = country_name
    )
  }
  
  # Add train/test indicator
  df_plot$Period <- ifelse(seq_along(covid_series) <= split_index, "Train", "Test")
  
  return(df_plot)
}

calculate_metrics_bm_test <- function(country_name, df, test_split = 0.2) {
  # Filter data for the specific country
  df_country <- df %>% filter(country == country_name)
  covid_series <- df_country$new_cases
  covid_series_cum <- df_country$cases
  
  # Define train-test split index
  split_index <- floor((1 - test_split) * length(covid_series))
  
  # Split data into training and test sets
  train_series <- covid_series[1:split_index]
  test_series <- covid_series[(split_index + 1):length(covid_series)]
  test_series_cum <- covid_series_cum[(split_index + 1):length(covid_series_cum)]
  
  # Train BM model only on the training set
  bm_model <- BM(train_series, display = FALSE)
  
  # Forecast only for the test period
  test_forecast <- predict(bm_model, newx = (split_index + 1):length(covid_series))
  
  # Compute RMSE and R² based on cumulative values
  residuals <- test_series_cum - test_forecast
  ss_res <- sum(residuals^2)
  ss_tot <- sum((test_series_cum - mean(test_series_cum))^2)
  
  # Calculate metrics
  r2 <- 1 - (ss_res / ss_tot)
  rmse <- sqrt(mean(residuals^2))
  
  return(c(R2 = r2, RMSE = rmse))
}

calculate_metrics_ggm_test <- function(country_name, df, test_split = 0.2) {
  # Filter data for the specific country
  df_country <- df %>% filter(country == country_name)
  covid_series <- df_country$new_cases
  covid_series_cum <- df_country$cases
  
  # Define train-test split index
  split_index <- floor((1 - test_split) * length(covid_series))
  
  # Split data into training and test sets
  train_series <- covid_series[1:split_index]
  test_series_cum <- covid_series_cum[(split_index + 1):length(covid_series_cum)]
  
  # Initialize GGM model variable
  ggm_model <- NULL
  
  # Attempt first GGM model
  try_result <- try({
    ggm_model <- GGM(train_series, mt='base', display = FALSE)
  }, silent = TRUE)
  
  # Check if an error occurred
  if (inherits(try_result, "try-error")) {
    if (grepl("chol.default.*not positive", try_result)) {
      message("Cholesky decomposition error detected. Trying alternative model...")
      
      # Attempt alternative GGM method
      try_result_alt <- try({
        ggm_model <- GGM(train_series, mt=function(x) pchisq(x,10), display = FALSE)
      }, silent = TRUE)
      
      # If alternative method also fails, stop execution
      if (inherits(try_result_alt, "try-error")) {
        stop("Both GGM model attempts failed. Error: ", try_result_alt)
      }
    } else {
      stop("Unexpected error in GGM model: ", try_result)
    }
  }
  
  # Ensure the model was successfully created
  if (is.null(ggm_model)) {
    stop("Failed to fit GGM model.")
  }
  
  # Forecast only for the test period
  test_forecast <- predict(ggm_model, newx = (split_index + 1):length(covid_series))
  
  # Compute RMSE and R² based on cumulative values
  residuals <- test_series_cum - test_forecast
  ss_res <- sum(residuals^2)
  ss_tot <- sum((test_series_cum - mean(test_series_cum))^2)
  
  # Calculate metrics
  r2 <- 1 - (ss_res / ss_tot)
  rmse <- sqrt(mean(residuals^2))
  
  return(c(R2 = r2, RMSE = rmse))
}
calculate_metrics_ggm_test('Germany', df_w, test_split = 0.2)

# Function to process each country
# returns residuals for train and test
ggm_residuals_test <- function(country_name, df, test_split = 0.2) {
  # Filter data for the specific country
  df_country <- df %>% filter(country == country_name)
  covid_series <- df_country$new_cases
  covid_series_cum <- df_country$cases
  
  # Define train-test split index
  split_index <- floor((1 - test_split) * length(covid_series))
  
  # Split data into training and test sets
  train_series <- covid_series[1:split_index]
  test_series_cum <- covid_series_cum[(split_index + 1):length(covid_series_cum)]
  
  # Initialize GGM model variable
  ggm_model <- NULL
  
  # Attempt first GGM model
  try_result <- try({
    ggm_model <- GGM(train_series, mt='base', display = FALSE)
  }, silent = TRUE)
  
  # Check if an error occurred
  if (inherits(try_result, "try-error")) {
    if (grepl("chol.default.*not positive", try_result)) {
      message("Cholesky decomposition error detected. Trying alternative model...")
      
      # Attempt alternative GGM method
      try_result_alt <- try({
        ggm_model <- GGM(train_series, mt=function(x) pchisq(x,10), display = FALSE)
      }, silent = TRUE)
      
      # If alternative method also fails, stop execution
      if (inherits(try_result_alt, "try-error")) {
        stop("Both GGM model attempts failed. Error: ", try_result_alt)
      }
    } else {
      stop("Unexpected error in GGM model: ", try_result)
    }
  }
  
  # Ensure the model was successfully created
  if (is.null(ggm_model)) {
    stop("Failed to fit GGM model.")
  }
  
  # Forecast for the training period
  fitted_train <- predict(ggm_model, newx = 1:split_index)
  
  # Forecast only for the test period
  fitted_test <- predict(ggm_model, newx = (split_index + 1):length(covid_series))
  
  # Calculate residuals for the training set
  residuals_train <- covid_series_cum[1:split_index] - fitted_train
  
  # Create a dataframe with train residuals for ARIMA model fitting
  df_train_residuals <- data.frame(
    Time = 1:split_index,
    Residuals = residuals_train,
    Cases = cumsum(train_series),
    Fitted = fitted_train
  )
  
  # Create and return a dataframe with test fitted values and actual test values
  df_test <- data.frame(
    Time = (split_index + 1):length(covid_series),
    Fitted = fitted_test,
    Cases = test_series_cum  # actual cases for the test period
  )
  
  return(list(train_residuals = df_train_residuals, test_data = df_test))
}

process_country_test <- function(country, df, test_split = 0.2) {
  # Step 1: Get GGM residuals and test data
  ggm_results <- ggm_residuals_test(country, df, test_split = test_split)
  df_train <- ggm_results$train_residuals
  df_test <- ggm_results$test_data
  
  # Step 2: Fit an ARIMA model on the cumulative cases from the train set using GGM fitted values as external regressor
  arima_model <- auto.arima(df_train$Cases, xreg = df_train$Fitted)
  
  # Step 3: Forecast cumulative cases for the test period using ARIMA model
  arima_forecast <- forecast(arima_model, h = nrow(df_test), xreg = df_test$Fitted)$mean
  
  # Step 4: Create full dataset for visualization
  plot_data <- data.frame(
    Time = c(df_train$Time, df_test$Time),  # Full time range (train + test)
    Cases = c(df_train$Cases, df_test$Cases),  # Actual full series
    Fitted = c(rep(NA, length(df_train$Cases)), arima_forecast)  # Show forecast only in test period
  )
  
  # Step 5: Generate the plot
  plot <- ggplot(plot_data, aes(x = Time)) +
    geom_line(aes(y = Cases, color = "Actual"), linewidth = 1) +  # Full actual series
    geom_line(aes(y = Fitted, color = "Forecast"), linewidth = 1, linetype = "dashed") +  # Forecast only for test period
    labs(
      title = paste(country),
      x = "Time",
      y = "Cumulative Cases",
      color = "Legend"
    ) +
    theme_minimal() +
    scale_color_manual(values = c("Actual" = "black", "Forecast" = "red")) +
    theme(
      legend.position = c(0.9, 0.1)  # Bottom right
    )
  
  return(plot)
}

save_ggm_r_test <- function(country, df, test_split = 0.2) {
  # Step 1: Get GGM residuals and test data
  ggm_results <- ggm_residuals_test(country, df, test_split = test_split)
  df_train <- ggm_results$train_residuals
  df_test <- ggm_results$test_data
  
  # Subset dates to match only rows used in the model
  total_rows <- nrow(df_train) + nrow(df_test)
  dates <- df %>%
    filter(country == country) %>%
    slice_head(n = total_rows) %>%
    pull(date)
  
  # Step 2: Fit ARIMA model
  arima_model <- auto.arima(df_train$Cases, xreg = df_train$Fitted)
  
  # Step 3: Get in-sample + forecasted fitted values
  fitted_train <- fitted(arima_model)
  forecast_test <- forecast(arima_model, h = nrow(df_test), xreg = df_test$Fitted)$mean
  full_fitted <- c(fitted_train, as.numeric(forecast_test))
  
  # Return both DATE and Fitted
  return(data.frame(DATE = dates, Fitted = full_fitted))
}


process_country_metrics_test <- function(country, df, test_split = 0.2) {
  # Step 1: Get GGM residuals and test data
  ggm_results <- ggm_residuals_test(country, df, test_split = test_split)
  df_train <- ggm_results$train_residuals
  df_test <- ggm_results$test_data
  
  # Step 2: Fit an ARIMA model on the training set cumulative cases with GGM fitted values as an external regressor
  arima_model <- auto.arima(df_train$Cases, xreg = df_train$Fitted)
  
  # Step 3: Forecast cumulative cases for the test period using ARIMA with GGM as xreg
  arima_forecast <- forecast(arima_model, h = nrow(df_test), xreg = df_test$Fitted)$mean
  
  # Step 4: Compute R² and RMSE for the test set
  actual <- df_test$Cases  # Actual test set cumulative cases
  predicted <- arima_forecast  # ARIMA forecasted test values
  
  # R² calculation
  ss_total <- sum((actual - mean(actual))^2)
  ss_residual <- sum((actual - predicted)^2)
  r_squared <- 1 - (ss_residual / ss_total)
  
  # RMSE calculation
  rmse <- sqrt(mean((actual - predicted)^2))
  
  # Step 5: Return results as a data frame
  result <- data.frame(
    Country = country,
    R2 = r_squared,
    RMSE = rmse
  )
  
  return(result)
}
## 16.1 BM----------------------


# Loop through the first 10 countries (to fit 2x5 grid)
plot_list <- lapply(unique_countries, function(country) {
  plot_bm_model_test(country, df_w, 0.2,TRUE)
})


# Arrange plots in a 2x5 grid
grid.arrange(grobs = plot_list, nrow = 2, ncol = 5)

### 16.1.2 Save Fitted Values------------
# Get fitted values for all countries
country_fitted_values <- lapply(unique_countries, function(country) {
  result <- save_bm_model_test(country_name = country, 
                               df = df_w, 
                               test_split = 0.2, 
                               cumulative = TRUE)
  return(result$Fitted)
})

# Convert to dataframe and set column names
fitted_df <- as.data.frame(do.call(cbind, country_fitted_values))
colnames(fitted_df) <- unique_countries

# Add dates as the first column (taken from the first country's results)
first_country_result <- save_bm_model_test(unique_countries[1], df_w, 0.2, FALSE)
fitted_df <- cbind(Date = first_country_result$Time, fitted_df)
# Assuming you've created the fitted_df as shown previously
write.csv(fitted_df, file = "../../results/BM_covid.csv", row.names = FALSE)


## 16.2 GGM---------------------------------

# Loop through the first 10 countries (to fit 2x5 grid)
plot_list <- lapply(unique_countries, function(country) {
  plot_ggm_model_test(country, df_w, 0.2,TRUE)
})

# Arrange plots in a 2x5 grid
grid.arrange(grobs = plot_list, nrow = 2, ncol = 5)

### 16.2.2 Save Fitted Values----------------
country_fitted_values <- lapply(unique_countries, function(country) {
  result <- save_ggm_model_test(country_name = country, 
                               df = df_w, 
                               test_split = 0.2, 
                               cumulative = TRUE)
  return(result$Fitted)
})

# Convert to dataframe and set column names
fitted_df <- as.data.frame(do.call(cbind, country_fitted_values))
colnames(fitted_df) <- unique_countries

# Add dates as the first column (taken from the first country's results)
first_country_result <- save_bm_model_test(unique_countries[1], df_w, 0.2, FALSE)
fitted_df <- cbind(Date = first_country_result$Time, fitted_df)
# write the csv
write.csv(fitted_df, file = "../../results/GGM_covid.csv", row.names = FALSE)


## 16.3 BM vs GGM Metrics-----------------------------
bm_metrics_test <- data.frame(Country = unique_countries, R2 = NA, RMSE = NA)
ggm_metrics_test <- data.frame(Country = unique_countries, R2 = NA, RMSE = NA)

# Loop through the countries and calculate metrics for BM
bm_metrics_test$R2 <- sapply(unique_countries, function(country) {
  calculate_metrics_bm_test(country, df_w, 0.2)[1]
})
bm_metrics_test$RMSE <- sapply(unique_countries, function(country) {
  calculate_metrics_bm_test(country, df_w, 0.2)[2]
})

# Loop through the countries and calculate metrics for GGM
ggm_metrics_test$R2 <- sapply(unique_countries, function(country) {
  calculate_metrics_ggm_test(country, df_w, 0.2)[1]
})
ggm_metrics_test$RMSE <- sapply(unique_countries, function(country) {
  calculate_metrics_ggm_test(country, df_w, 0.2)[2]
})

# Combine BM and GGM metrics into one table
combined_metrics <- merge(bm_metrics_test, ggm_metrics_test, by = "Country", suffixes = c("_BM", "_GGM"))

# Display the table
View(combined_metrics)




# 17. GGM + Refinement-----------------
# all done with 80%-20% train test split
## 17.1 Daily-------------------
# Iterate over each country and process
plot_list <- lapply(unique_countries, function(country) {
  process_country_test(country, df_d)
})

# Arrange plots in a 2x5 grid
grid.arrange(grobs = plot_list, nrow = 2, ncol = 5)

## 17.2 Weekly-------------------
# Iterate over each country and process
plot_list <- lapply(unique_countries, function(country) {
  process_country_test(country, df_w)
})

# Arrange plots in a 2x5 grid
grid.arrange(grobs = plot_list, nrow = 2, ncol = 5)

### 17.2.2 Save fitted values-------------


# Initialize an empty list to store the results
fitted_values_list <- list()

# Loop over each country in unique_countries
for (country in unique_countries) {
  # Call the save_ggm_r_test function for the current country
  fitted_values <- save_ggm_r_test(country, df = df_w, test_split = 0.2)
  
  # Add the country name as a column in the dataframe
  fitted_values$Country <- country
  
  # Append the result to the list
  fitted_values_list[[country]] <- fitted_values
}

# Combine all dataframes in the list into a single dataframe
fitted_values_df <- do.call(rbind, fitted_values_list)

# Print the resulting dataframe
View(fitted_values_df)




# Reshape the fitted_values_df dataframe
fitted_values_wide <- fitted_values_df %>%
  pivot_wider(names_from = Country, values_from = Fitted)

# Print the resulting dataframe
View(fitted_values_wide)

# Specify the file path where you want to save the CSV file
file_path <- "../../results/GGM_Arima_covid.csv"  # Replace with your desired file path

# Save the fitted_values_df dataframe to a CSV file
write.csv(fitted_values_wide, file = file_path, row.names = FALSE)


## 17.3 Monthly-------------------
# Iterate over each country and process
plot_list <- lapply(unique_countries, function(country) {
  process_country_test(country, df_m)
})

# Arrange plots in a 2x5 grid
grid.arrange(grobs = plot_list, nrow = 2, ncol = 5)


# 18. GGM + Refinement Metrics--------------
## 18.1 Daily------------

metrics_df <- lapply(unique_countries, function(country) {
  process_country_metrics_test(country, df_d)
}) %>% bind_rows()

# Print the resulting data frame
print(metrics_df)
View(metrics_df)

## 18.2 Weekly------------

metrics_df <- lapply(unique_countries, function(country) {
  process_country_metrics_test(country, df_w)
}) %>% bind_rows()

# Print the resulting data frame

View(metrics_df)


## 18.3 Monthly------------

metrics_df <- lapply(unique_countries, function(country) {
  process_country_metrics_test(country, df_m)
}) %>% bind_rows()

# Print the resulting data frame
View(metrics_df)


#EPIDEMICS*******************************-------------------------
# 19. Adjust Functions------------------------
plot_bm_epidemics_test <- function(time_series, test_split = 0.2, cumulative = FALSE) {
  # Filter data for the specific country
  time_series <- time_series
  
  # Define train-test split index
  split_index <- floor((1 - test_split) * length(time_series))
  
  # Split data into training and test sets
  train_series <- time_series[1:split_index]
  test_series <- time_series[(split_index + 1):length(time_series)]
  
  # Train BM model on training set
  bm_model <- BM(train_series, display = FALSE)
  
  # Forecast for the entire period
  full_forecast <- predict(bm_model, newx = 1:length(time_series))
  
  # Compute instantaneous fitted values
  full_forecast_inst <- make.instantaneous(full_forecast)
  
  # Prepare data for plotting (show only test set forecast)
  if (cumulative == FALSE) {
    df_plot <- data.frame(
      Time = 1:length(time_series),
      Actual = time_series,
      Fitted = c(rep(NA, split_index), full_forecast_inst[(split_index + 1):length(time_series)])  # Only show test forecast
    )
  } else {
    df_plot <- data.frame(
      Time = 1:length(time_series),
      Actual = cumsum(time_series),
      Fitted = c(rep(NA, split_index), full_forecast[(split_index + 1):length(time_series)])  # Only show test forecast
    )
  }
  
  # Create plot
  p <- ggplot(df_plot, aes(x = Time)) +
    geom_line(aes(y = Actual, color = "Actual"), size = 1) +
    geom_line(aes(y = Fitted, color = "Fitted"), linetype = "dashed", size = 1) +
    scale_color_manual(values = c("Actual" = "black", "Fitted" = "red")) +
    labs(color = "Legend") +  # Legend title
    ggtitle(paste("Cases")) +
    ylab("Cases") + 
    xlab("Time") +
    theme_minimal() +
    theme(
      text = element_text(size = 8),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 8),
      plot.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8),
      plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")
    )
  
  return(p)
}


save_bm_epidemics_test <- function(time_series, dates, test_split = 0.2, cumulative = FALSE) {
  # Define train-test split index
  split_index <- floor((1 - test_split) * length(time_series))
  
  # get the dates from the time series
  if (missing(dates)) {
    # If dates not provided, create a sequence of weekly dates starting from today
    dates <- seq.Date(from = Sys.Date() - 7 * (length(time_series) - 1), 
                      by = "week", 
                      length.out = length(time_series))
  } else if (length(dates) != length(time_series)) {
    stop("Length of dates must match length of time_series")
  }
  
  # Split data into training and test sets
  train_series <- time_series[1:split_index]
  test_series <- time_series[(split_index + 1):length(time_series)]
  
  # Train BM model on training set
  bm_model <- BM(train_series, display = FALSE)
  
  # Forecast for the entire period
  full_forecast <- predict(bm_model, newx = 1:length(time_series))
  
  # Compute instantaneous fitted values
  full_forecast_inst <- make.instantaneous(full_forecast)
  
  # Prepare data for plotting (show only test set forecast)
  if (cumulative == FALSE) {
    df_plot <- data.frame(
      Date = dates,
      Fitted = full_forecast_inst
    )
  } else {
    df_plot <- data.frame(
      Date = dates,
      Fitted = full_forecast
    )
  }
  
  return(df_plot)
}



plot_ggm_epidemics_test <- function( time_series, test_split = 0.2, cumulative = FALSE) {
  
  time_series <- time_series
  
  # Define train-test split index
  split_index <- floor((1 - test_split) * length(time_series))
  
  # Split data into training and test sets
  train_series <- time_series[1:split_index]
  test_series <- time_series[(split_index + 1):length(time_series)]
  
  # Initialize the GGM model
  ggm_model <- NULL
  
  # Attempt first GGM model
  try_result <- try({
    ggm_model <- GGM(train_series, display = FALSE)
  }, silent = TRUE)
  
  # Check if an error occurred
  if (inherits(try_result, "try-error")) {
    if (grepl("chol.default.*not positive", try_result)) {
      message("Cholesky decomposition error detected. Trying alternative model...")
      
      # Attempt alternative GGM method
      try_result_alt <- try({
        ggm_model <- GGM(train_series, mt=function(x) pchisq(x,10), display = FALSE)
      }, silent = TRUE)
      
      # If alternative method also fails, stop execution
      if (inherits(try_result_alt, "try-error")) {
        stop("Both GGM model attempts failed. Error: ", try_result_alt)
      }
    } else {
      stop("Unexpected error in GGM model: ", try_result)
    }
  }
  
  # Ensure the model was successfully created
  if (is.null(ggm_model)) {
    stop("Failed to fit GGM model.")
  }
  
  # Forecast for the entire period
  full_forecast <- predict(ggm_model, newx = 1:length(time_series))
  
  # Compute instantaneous fitted values
  full_forecast_inst <- make.instantaneous(full_forecast)
  
  # Prepare data for plotting (only test set forecast is plotted)
  if (cumulative == FALSE) {
    df_plot <- data.frame(
      Time = 1:length(time_series),
      Actual = time_series,
      Fitted = c(rep(NA, split_index), full_forecast_inst[(split_index + 1):length(time_series)]) # Only show test forecast
    )
  } else {
    df_plot <- data.frame(
      Time = 1:length(time_series),
      Actual = cumsum(time_series),
      Fitted = c(rep(NA, split_index), full_forecast[(split_index + 1):length(time_series)]) # Only show test forecast
    )
  }
  
  # Create plot
  p <- ggplot(df_plot, aes(x = Time)) +
    geom_line(aes(y = Actual), color = "black", size = 1) +
    geom_line(aes(y = Fitted), color = "red", linetype = "dashed", size = 1) +
    ggtitle(paste("Cases")) +
    ylab("Cases") + 
    xlab("Time") +
    theme_minimal()
  
  return(p)
}


save_ggm_epidemics_test <- function(time_series,
                                    dates = NULL,
                                    test_split = 0.2,
                                    cumulative = FALSE) {
  
  # --- 1. train‑test split 
  split_index  <- floor((1 - test_split) * length(time_series))
  train_series <- time_series[1:split_index]
  test_series  <- time_series[(split_index + 1):length(time_series)]  # <- unused but harmless
  
  # --- 2. date vector
  if (is.null(dates)) {
    dates <- seq.Date(from = Sys.Date() - 7 * (length(time_series) - 1),
                      by   = "week",
                      length.out = length(time_series))
  } else if (length(dates) != length(time_series)) {
    stop("Length of dates must match length of time_series")
  }
  
  # --- 3. fit GGM -
  ggm_model <- tryCatch(
    GGM(train_series, display = FALSE),
    error = function(e) {
      if (grepl("chol.default.*not positive", e$message)) {
        message("Cholesky error, trying alternative m‑t function …")
        GGM(train_series, mt = function(x) pchisq(x, 10), display = FALSE)
      } else {
        stop(e)
      }
    }
  )
  
  # --- 4. forecast 
  full_forecast      <- predict(ggm_model, newx = 1:length(time_series))
  full_forecast_inst <- make.instantaneous(full_forecast)
  
  # --- 5. assemble output
  data.frame(
    Date   = dates,
    Fitted = if (cumulative) full_forecast else full_forecast_inst
  )
}


calculate_metrics_bm_epidemics_test <- function(time_series, test_split = 0.2) {
  # Filter data for the specific country
  
  time_series <- time_series
  time_series_cum <- cumsum(time_series)
  
  # Define train-test split index
  split_index <- floor((1 - test_split) * length(time_series))
  
  # Split data into training and test sets
  train_series <- time_series[1:split_index]
  test_series <- time_series[(split_index + 1):length(time_series)]
  test_series_cum <- time_series_cum[(split_index + 1):length(time_series_cum)]
  
  # Train BM model only on the training set
  bm_model <- BM(train_series, display = FALSE)
  
  # Forecast only for the test period
  test_forecast <- predict(bm_model, newx = (split_index + 1):length(time_series))
  
  # Compute RMSE and R² based on cumulative values
  residuals <- test_series_cum - test_forecast
  ss_res <- sum(residuals^2)
  ss_tot <- sum((test_series_cum - mean(test_series_cum))^2)
  
  # Calculate metrics
  r2 <- 1 - (ss_res / ss_tot)
  rmse <- sqrt(mean(residuals^2))
  
  return(c(R2 = r2, RMSE = rmse))
}

calculate_metrics_ggm_epidemics_test <- function(time_series, test_split = 0.2) {
  
  time_series <- time_series
  time_series_cum <- cumsum(time_series)
  
  # Define train-test split index
  split_index <- floor((1 - test_split) * length(time_series))
  
  # Split data into training and test sets
  train_series <- time_series[1:split_index]
  test_series_cum <- time_series_cum[(split_index + 1):length(time_series)]
  
  # Initialize GGM model variable
  ggm_model <- NULL
  
  # Attempt first GGM model
  try_result <- try({
    ggm_model <- GGM(train_series, mt='base', display = FALSE)
  }, silent = TRUE)
  
  # Check if an error occurred
  if (inherits(try_result, "try-error")) {
    if (grepl("chol.default.*not positive", try_result)) {
      message("Cholesky decomposition error detected. Trying alternative model...")
      
      # Attempt alternative GGM method
      try_result_alt <- try({
        ggm_model <- GGM(train_series, mt=function(x) pchisq(x,10), display = FALSE)
      }, silent = TRUE)
      
      # If alternative method also fails, stop execution
      if (inherits(try_result_alt, "try-error")) {
        stop("Both GGM model attempts failed. Error: ", try_result_alt)
      }
    } else {
      stop("Unexpected error in GGM model: ", try_result)
    }
  }
  
  # Ensure the model was successfully created
  if (is.null(ggm_model)) {
    stop("Failed to fit GGM model.")
  }
  
  # Forecast only for the test period
  test_forecast <- predict(ggm_model, newx = (split_index + 1):length(time_series))
  
  # Compute RMSE and R² based on cumulative values
  residuals <- test_series_cum - test_forecast
  ss_res <- sum(residuals^2)
  ss_tot <- sum((test_series_cum - mean(test_series_cum))^2)
  
  # Calculate metrics
  r2 <- 1 - (ss_res / ss_tot)
  rmse <- sqrt(mean(residuals^2))
  
  return(c(R2 = r2, RMSE = rmse))
}


# returns residuals for train and test
ggm_residuals_epidemics_test <- function(time_series, test_split = 0.2) {
  
  time_series <- time_series
  time_series_cum <- cumsum(time_series)
  
  # Define train-test split index
  split_index <- floor((1 - test_split) * length(time_series))
  
  # Split data into training and test sets
  train_series <- time_series[1:split_index]
  test_series_cum <- time_series_cum[(split_index + 1):length(time_series_cum)]
  
  # Initialize GGM model variable
  ggm_model <- NULL
  
  # Attempt first GGM model
  try_result <- try({
    ggm_model <- GGM(train_series, mt='base', display = FALSE)
  }, silent = TRUE)
  
  # Check if an error occurred
  if (inherits(try_result, "try-error")) {
    if (grepl("chol.default.*not positive", try_result)) {
      message("Cholesky decomposition error detected. Trying alternative model...")
      
      # Attempt alternative GGM method
      try_result_alt <- try({
        ggm_model <- GGM(train_series, mt=function(x) pchisq(x,10), display = FALSE)
      }, silent = TRUE)
      
      # If alternative method also fails, stop execution
      if (inherits(try_result_alt, "try-error")) {
        stop("Both GGM model attempts failed. Error: ", try_result_alt)
      }
    } else {
      stop("Unexpected error in GGM model: ", try_result)
    }
  }
  
  # Ensure the model was successfully created
  if (is.null(ggm_model)) {
    stop("Failed to fit GGM model.")
  }
  
  # Forecast for the training period
  fitted_train <- predict(ggm_model, newx = 1:split_index)
  
  # Forecast only for the test period
  fitted_test <- predict(ggm_model, newx = (split_index + 1):length(time_series))
  
  # Calculate residuals for the training set
  residuals_train <- time_series_cum[1:split_index] - fitted_train
  
  # Create a dataframe with train residuals for ARIMA model fitting
  df_train_residuals <- data.frame(
    Time = 1:split_index,
    Residuals = residuals_train,
    Cases = cumsum(train_series),
    Fitted = fitted_train
  )
  
  # Create and return a dataframe with test fitted values and actual test values
  df_test <- data.frame(
    Time = (split_index + 1):length(time_series),
    Fitted = fitted_test,
    Cases = test_series_cum  # actual cases for the test period
  )
  
  return(list(train_residuals = df_train_residuals, test_data = df_test))
}

process_epidemics_test <- function(time_series, test_split = 0.2) {
  # Step 1: Get GGM residuals and test data
  ggm_results <- ggm_residuals_epidemics_test(time_series, test_split = test_split)
  df_train <- ggm_results$train_residuals
  df_test <- ggm_results$test_data
  
  # Step 2: Fit an ARIMA model on the cumulative cases from the train set using GGM fitted values as external regressor
  arima_model <- auto.arima(df_train$Cases, xreg = df_train$Fitted)
  
  # Step 3: Forecast cumulative cases for the test period using ARIMA model
  arima_forecast <- forecast(arima_model, h = nrow(df_test), xreg = df_test$Fitted)$mean
  
  # Step 4: Create full dataset for visualization
  plot_data <- data.frame(
    Time = c(df_train$Time, df_test$Time),  # Full time range (train + test)
    Cases = c(df_train$Cases, df_test$Cases),  # Actual full series
    Fitted = c(rep(NA, length(df_train$Cases)), arima_forecast)  # Show forecast only in test period
  )
  
  # Step 5: Generate the plot
  plot <- ggplot(plot_data, aes(x = Time)) +
    geom_line(aes(y = Cases, color = "Actual"), linewidth = 1) +  # Full actual series
    geom_line(aes(y = Fitted, color = "Forecast"), linewidth = 1, linetype = "dashed") +  # Forecast only for test period
    labs(
      title = 'Epidemic Cases',
      x = "Time",
      y = "Cumulative Cases",
      color = "Legend"
    ) +
    theme_minimal() +
    scale_color_manual(values = c("Actual" = "black", "Forecast" = "red")) +
    theme(
      legend.position = c(0.9, 0.1)  # Bottom right
    )
  
  return(plot)
}

process_epidemics_metrics_test <- function(time_series, test_split = 0.2) {
  # Step 1: Get GGM residuals and test data
  ggm_results <- ggm_residuals_epidemics_test(time_series, test_split = test_split)
  df_train <- ggm_results$train_residuals
  df_test <- ggm_results$test_data
  
  # Step 2: Fit an ARIMA model on the training set cumulative cases with GGM fitted values as an external regressor
  arima_model <- auto.arima(df_train$Cases, xreg = df_train$Fitted)
  
  # Step 3: Forecast cumulative cases for the test period using ARIMA with GGM as xreg
  arima_forecast <- forecast(arima_model, h = nrow(df_test), xreg = df_test$Fitted)$mean
  
  # Step 4: Compute R² and RMSE for the test set
  actual <- df_test$Cases  # Actual test set cumulative cases
  predicted <- arima_forecast  # ARIMA forecasted test values
  
  # R² calculation
  ss_total <- sum((actual - mean(actual))^2)
  ss_residual <- sum((actual - predicted)^2)
  r_squared <- 1 - (ss_residual / ss_total)
  
  # RMSE calculation
  rmse <- sqrt(mean((actual - predicted)^2))
  
  # Step 5: Return results as a data frame
  result <- data.frame(
    R2 = r_squared,
    RMSE = rmse
  )
  
  return(result)
}


# save ggm + refinement
# --------------------------------------------------------------------------- #
# Helper : turn a weekly ts or plain vector into a Date vector                #
# --------------------------------------------------------------------------- #
get_dates_from_ts <- function(x) {
  
  # 1. plain numeric / ts without frequency 
  if (!is.ts(x)) {
    return(seq.Date(Sys.Date() - 7 * (length(x) - 1),
                    by = "week", length.out = length(x)))
  }
  
  freq <- frequency(x)
  if (!(freq %in% c(52, 53))) {          # not weekly → same fallback
    warning("ts object is not weekly; generating dates from today's week.")
    return(seq.Date(Sys.Date() - 7 * (length(x) - 1),
                    by = "week", length.out = length(x)))
  }
  
  # 2. weekly ts : need ISO week → Date 
  if (!requireNamespace("ISOweek", quietly = TRUE)) {
    warning("Package 'ISOweek' not installed; falling back to simple sequence.")
    return(seq.Date(Sys.Date() - 7 * (length(x) - 1),
                    by = "week", length.out = length(x)))
  }
  
  st <- start(x)                  # e.g. c(2005, 10)
  year0 <- st[1]
  week0 <- st[2]
  
  # ISO week starts on Monday (“‑1”)
  start_date <- ISOweek::ISOweek2date(sprintf("%d-W%02d-1", year0, week0))
  seq.Date(from = start_date, by = "week", length.out = length(x))
}

# --------------------------------------------------------------------------- #
# 1.  Fit GGM on TRAIN incidence and return residuals + dates                 #
# --------------------------------------------------------------------------- #
ggm_residuals_epidemics_test_2 <- function(time_series, test_split = 0.20) {
  
  dates_vec <- get_dates_from_ts(time_series)        # real Date vector
  ts_cum    <- cumsum(as.numeric(time_series))
  split_idx <- floor((1 - test_split) * length(time_series))
  
  train_inc <- time_series[1:split_idx]
  test_cum  <- ts_cum[(split_idx + 1):length(ts_cum)]
  
  ggm_model <- tryCatch(
    GGM(train_inc, mt = "base", display = FALSE),
    error = function(e) {
      if (grepl("chol.default.*not positive", e$message)) {
        message("Cholesky error — trying alternative m‑t …")
        GGM(train_inc, mt = function(x) stats::pchisq(x, 10), display = FALSE)
      } else stop(e)
    }
  )
  
  ggm_fit_train <- predict(ggm_model, newx = 1:split_idx)
  ggm_fit_test  <- predict(ggm_model, newx = (split_idx + 1):length(time_series))
  
  list(
    train = data.frame(Date   = dates_vec[1:split_idx],
                       Fitted = ggm_fit_train,
                       Cases  = ts_cum[1:split_idx]),  # still needed for ARIMA
    test  = data.frame(Date   = dates_vec[(split_idx + 1):length(ts_cum)],
                       Fitted = ggm_fit_test,
                       Cases  = test_cum)
  )
}

# --------------------------------------------------------------------------- #
# 2.  ARIMA on residuals — return ONLY Date + Fitted (+ Set flag)             #
# --------------------------------------------------------------------------- #
save_epidemics_ggm_r <- function(time_series, test_split = 0.20) {
  
  ggm <- ggm_residuals_epidemics_test_2(time_series, test_split)
  tr  <- ggm$train
  te  <- ggm$test
  
  arima_mod <- forecast::auto.arima(tr$Cases, xreg = tr$Fitted)
  
  fitted_train <- stats::fitted(arima_mod)
  fore_test    <- forecast::forecast(arima_mod,
                                     h    = nrow(te),
                                     xreg = te$Fitted)$mean
  
  out <- data.frame(
    Date   = c(tr$Date, te$Date),
    Fitted = c(as.numeric(fitted_train), as.numeric(fore_test)),
    Set    = c(rep("Train", nrow(tr)),   rep("Test",  nrow(te)))
  )
  rownames(out) <- NULL
  out
}


# 20. BM Epidemics-------------------------

# List of 4 time series
time_series_list <- list(ts_casos_dengue, 
                         ts_casos_zika, 
                         ts_casos_chic, 
                         ts_casos_var)


# Generate plots
plot1 <- plot_bm_epidemics_test(time_series_list[[1]], test_split = 0.2, cumulative = TRUE)
plot2 <- plot_bm_epidemics_test(time_series_list[[2]], test_split = 0.2, cumulative = TRUE)
plot3 <- plot_bm_epidemics_test(time_series_list[[3]], test_split = 0.2, cumulative = TRUE)
plot4 <- plot_bm_epidemics_test(time_series_list[[4]], test_split = 0.2, cumulative = TRUE)


# Combine plots in a 2x2 grid using gridExtra
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)


## 20.1 Save csv-----------------
# Define the series list
series_list <- list(
  Dengue = ts_casos_dengue,
  Zika = ts_casos_zika,
  Chicungunya = ts_casos_chic,
  Varicela = ts_casos_var
)


# Modified processing to include actual values
results_list <- lapply(names(series_list), function(disease_name) {
  ts_data <- series_list[[disease_name]]
  
  # Get dates (consistent with your function logic)
  dates <- seq.Date(from = Sys.Date() - 7 * (length(ts_data) - 1), 
                    by = "week", 
                    length.out = length(ts_data))
  
  data.frame(
    Date = dates,
    Disease = disease_name,
    Actual = ts_data,
    Fitted = save_bm_epidemics_test(ts_data, cumulative = TRUE)$Fitted
  )
})

# Create separate wide dataframes for actual and fitted values
actual_wide <- bind_rows(results_list) %>%
  select(Date, Disease, Actual) %>%
  pivot_wider(names_from = Disease, values_from = Actual)

fitted_wide <- bind_rows(results_list) %>%
  select(Date, Disease, Fitted) %>%
  pivot_wider(names_from = Disease, values_from = Fitted)

# Specify the file path where you want to save the CSV file
file_path <- "../../results/BM_epidemics.csv"  # Replace with your desired file path

# Save the fitted_values_df dataframe to a CSV file
write.csv(fitted_wide, file = file_path, row.names = FALSE)

# 21. GGM Epidemics----------------------

# List of 4 time series


# Generate plots
plot12 <- plot_ggm_epidemics_test(time_series_list[[1]], test_split = 0.2, cumulative = TRUE)
plot22 <- plot_ggm_epidemics_test(time_series_list[[2]], test_split = 0.2, cumulative = TRUE)
plot32 <- plot_ggm_epidemics_test(time_series_list[[3]], test_split = 0.2, cumulative = TRUE)
plot42 <- plot_ggm_epidemics_test(time_series_list[[4]], test_split = 0.2, cumulative = TRUE)


# Combine plots in a 2x2 grid using gridExtra
grid.arrange(plot12, plot22, plot32, plot42, ncol = 2)


## 21.1 Save csv----------------------

results_list <- map(names(series_list), function(disease_name) {
  ts_data <- series_list[[disease_name]]
  
  dates <- seq.Date(from = Sys.Date() - 7 * (length(ts_data) - 1),
                    by   = "week",
                    length.out = length(ts_data))
  
  fitted_vals <- save_ggm_epidemics_test(ts_data, dates = dates, cumulative =  TRUE)$Fitted
  stopifnot(length(ts_data) == length(fitted_vals))
  
  tibble(Date = dates,
         Disease = disease_name,
         Actual  = ts_data,
         Fitted  = fitted_vals)
})

actual_wide <- bind_rows(results_list) %>%
  select(Date, Disease, Actual) %>%
  pivot_wider(names_from = Disease, values_from = Actual)

fitted_wide <- bind_rows(results_list) %>%
  select(Date, Disease, Fitted) %>%
  pivot_wider(names_from = Disease, values_from = Fitted)

file_path <- "../../results/GGM_epidemics.csv"
if (!dir.exists(dirname(file_path)))
  dir.create(dirname(file_path), recursive = TRUE)

write.csv(fitted_wide, file = file_path, row.names = FALSE)
View(fitted_wide)

# 22. BM vs GGM Metrics Epidemics---------------

series_list <- list(
  Dengue = ts_casos_dengue,
  Zika = ts_casos_zika,
  Chicungunya = ts_casos_chic,
  Varicela = ts_casos_var
)

# Initialize a data frame to store the results
results <- data.frame(
  Series = character(),
  Model = character(),
  R2 = numeric(),
  RMSE = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each series and calculate metrics for both models
for (name in names(series_list)) {
  time_series <- series_list[[name]]
  
  # Calculate metrics for BM model
  bm_metrics <- calculate_metrics_bm_epidemics_test(time_series, test_split = 0.5)
  results <- rbind(results, data.frame(
    Series = name,
    Model = "BM",
    R2 = bm_metrics["R2"],
    RMSE = bm_metrics["RMSE"],
    stringsAsFactors = FALSE
  ))
  
  # Calculate metrics for GGM model
  ggm_metrics <- calculate_metrics_ggm_epidemics_test(time_series,  test_split = 0.5)
  results <- rbind(results, data.frame(
    Series = name,
    Model = "GGM",
    R2 = ggm_metrics["R2"],
    RMSE = ggm_metrics["RMSE"],
    stringsAsFactors = FALSE
  ))
}

# Print the results table
View(results)

# 23. GGM + Refinement---------------------

# Generate plots
plot13 <- process_epidemics_test(time_series_list[[1]], test_split = 0.2)
plot23 <- process_epidemics_test(time_series_list[[2]], test_split = 0.2)
plot33 <- process_epidemics_test(time_series_list[[3]], test_split = 0.2)
plot43 <- process_epidemics_test(time_series_list[[4]], test_split = 0.2)


# Combine plots in a 2x2 grid using gridExtra
grid.arrange(plot13, plot23, plot33, plot43, ncol = 2)


## 23.1 Save csv----------------------

# 1.  Run GGM → ARIMA for every disease and collect fitted values
results_list <- map(names(series_list), function(disease_name) {
  
  ts_data   <- series_list[[disease_name]]
  
  # save_epidemics_ggm_r() already returns Date + Fitted for the FULL series
  fit_df    <- save_epidemics_ggm_r(ts_data, test_split = 0.20)
  
  # sanity check (optional but nice to keep)
  stopifnot(nrow(fit_df) == length(ts_data))
  
  tibble(Date    = fit_df$Date,
         Disease = disease_name,
         Fitted  = fit_df$Fitted)          # <- no Actual, by request
})

# 2.  Wide format: one column per disease, Date down the rows
fitted_wide <- bind_rows(results_list) %>%
  pivot_wider(names_from = Disease, values_from = Fitted)

# ---------------------------------------------------------------------------
# 3.  Write to disk (create directory if it doesn’t exist)
# ---------------------------------------------------------------------------
file_path <- "../../results/GGM_ARIMA_epidemics.csv"
dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)

write.csv(fitted_wide, file = file_path, row.names = FALSE)

# optional: inspect in RStudio viewer
View(fitted_wide)




# 24. GGM + Refinement Epidemics Metrics----------------------
# Initialize a data frame to store the results
results_arima <- data.frame(
  Series = character(),
  R2 = numeric(),
  RMSE = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each series and calculate metrics using process_epidemic_metrics
for (name in names(series_list)) {
  time_series <- series_list[[name]]
  
  # Calculate metrics using process_epidemic_metrics
  metrics <- process_epidemics_metrics_test(time_series, test_split = 0.2)
  
  # Append the results to the data frame
  results_arima <- rbind(results_arima, data.frame(
    Series = name,
    R2 = metrics["R2"],
    RMSE = metrics["RMSE"],
    stringsAsFactors = FALSE
  ))
}

# Print the results table
View(results_arima)

