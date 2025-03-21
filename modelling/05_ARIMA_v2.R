rm(list=ls())
# Load libraries
library(forecast)
library(tseries)
library(ggplot2)
library(lubridate)
library(readr)
library(dplyr)
library(gridExtra)

setwd('GitHub/thesis/modelling/')

# 0. load data-------------------------
df_covid <- read_csv("../Data/silver/covid_data_weekly.csv")
df_dengue <- read_csv("../Data/silver/dengue_no_split.csv")
df_zika <- read_csv("../Data/silver/zika.csv")
df_chic <- read_csv("../Data/silver/chicunguya.csv")
df_var <- read_csv("../Data/silver/varicela.csv")

# 1. Wrangling--------------------------------

# Convert 'date' column to Date format
df_covid$date <- as.Date(df_covid$date)
df_dengue$date <- as.Date(df_dengue$DATE)
df_zika$date <- as.Date(df_zika$DATE)
df_chic$date <- as.Date(df_chic$DATE)
df_var$date <- as.Date(df_var$DATE)

# 2. Functions----------------
## 2.1 Covid-------------------------
# Function to plot ACF and PACF
plot_acf_pacf <- function(series, lags = 20) {
  par(mfrow = c(1, 2))  # Set layout for two plots side by side
  acf(series, lag.max = lags, main = "Autocorrelation Function (ACF)")
  pacf(series, lag.max = lags, main = "Partial Autocorrelation Function (PACF)")
  par(mfrow = c(1, 1))  # Reset layout
}

# Function to evaluate ARIMA models for different countries
evaluate_arima_single <- function(df, country) {
  tryCatch({
    # Filter data for the given country
    df_country <- df[df$country == country, ]
    df_country$Time <- seq_along(df_country$cases)  # Create a time index
    series <- ts(df_country$cases)
    
    # Fit ARIMA model
    model <- auto.arima(series)
    fitted_values <- fitted(model)
    
    # Calculate RMSE and R-squared
    rmse <- sqrt(mean((series - fitted_values)^2))
    r2 <- 1 - sum((series - fitted_values)^2) / sum((series - mean(series))^2)
    
    # Prepare data for ggplot
    df_plot <- data.frame(Time = df_country$Time, Actual = df_country$cases, Fitted = fitted_values)
    
    # Create ggplot
    p <- ggplot(df_plot, aes(x = Time)) +
      geom_line(aes(y = Actual, color = "Actual"), size = 1) +
      geom_line(aes(y = Fitted, color = "Fitted"), linetype = "dashed", size = 1) +
      scale_color_manual(values = c("Actual" = "black", "Fitted" = "red")) +
      labs(color = "Legend") +  # Legend title
      ggtitle(paste("COVID-19 Cases in", country)) +
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
    
    return(list(plot = p, result = data.frame(country = country, rmse = rmse, r2 = r2)))
  }, error = function(e) {
    print(paste("Error processing", country, ":", e$message))
    return(NULL)
  })
}

# with train-test split
evaluate_arima_single_train <- function(df, country, train_ratio = 0.8) {
  tryCatch({
    # Filter data for the given country
    df_country <- df[df$country == country, ]
    df_country$Time <- seq_along(df_country$cases)  # Create a time index
    series <- ts(df_country$cases)
    
    # Train-test split
    train_size <- floor(train_ratio * length(series))
    train_series <- series[1:train_size]
    test_series <- series[(train_size + 1):length(series)]
    
    # Fit ARIMA model using only the train data
    model <- auto.arima(train_series)
    
    # Get fitted values for the entire time range
    fitted_values <- c(fitted(model), forecast(model, h = length(test_series))$mean)
    
    # Calculate RMSE and R-squared for the test set
    actual_test <- test_series
    fitted_test <- fitted_values[(train_size + 1):length(fitted_values)]
    
    rmse <- sqrt(mean((actual_test - fitted_test)^2))
    r2 <- 1 - sum((actual_test - fitted_test)^2) / sum((actual_test - mean(actual_test))^2)
    
    # Prepare data for ggplot
    df_plot <- data.frame(Time = df_country$Time, Actual = df_country$cases, Fitted = fitted_values)
    
    # Create ggplot
    p <- ggplot(df_plot, aes(x = Time)) +
      geom_line(aes(y = Actual, color = "Actual"), size = 1) +
      geom_line(aes(y = Fitted, color = "Fitted"), linetype = "dashed", size = 1) +
      scale_color_manual(values = c("Actual" = "black", "Fitted" = "red")) +
      labs(color = "Legend") +  # Legend title
      ggtitle(paste("COVID-19 Cases in", country)) +
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
    
    return(list(plot = p, result = data.frame(country = country, rmse = rmse, r2 = r2)))
  }, error = function(e) {
    print(paste("Error processing", country, ":", e$message))
    return(NULL)
  })
}


# Function to generate a 2x5 grid of plots
evaluate_arima_grid <- function(df, countries) {
  results_list <- lapply(countries, function(country) {
    result <- evaluate_arima_single(df, country)
    return(result)
  })
  
  # Extract plots and results
  plot_list <- lapply(results_list, function(res) res$plot)
  results_df <- do.call(rbind, lapply(results_list, function(res) res$result))
  
  # Display plots
  grid.arrange(grobs = plot_list, nrow = 2, ncol = 5)
  
  # Return the results dataframe
  return(results_df)
}

# use the train-test split
# Function to generate a 2x5 grid of plots
evaluate_arima_grid_train <- function(df, countries) {
  results_list <- lapply(countries, function(country) {
    result <- evaluate_arima_single_train(df, country)
    return(result)
  })
  
  # Extract plots and results
  plot_list <- lapply(results_list, function(res) res$plot)
  results_df <- do.call(rbind, lapply(results_list, function(res) res$result))
  
  # Display plots
  grid.arrange(grobs = plot_list, nrow = 2, ncol = 5)
  
  # Return the results dataframe
  return(results_df)
}

## 2.2 Epidemics----------------

library(cowplot)

prepare_dataframe <- function(df) {
  df <- df %>%
    arrange(date) %>%
    mutate(cumulative_cases = cumsum(df$Casos))
  return(df)
}

# my custom arima

arima_epidemics <- function(df) {
  # order by date
  df <- df[order(df$date), ]
  # get the time series (cum cases)
  series <- ts(df$cumulative_cases, frequency = 52)
  
  #fit arima
  model <- auto.arima(series)
  fitted_values <- fitted(model)
  
  
  # Calculate RMSE and R-squared
  rmse <- sqrt(mean((series - fitted_values)^2))
  r2 <- 1 - sum((series - fitted_values)^2) / sum((series - mean(series))^2)
  
  # Prepare data for ggplot
  df_plot <- data.frame(
    Time = 1:length(series),
    Actual = series,
    Fitted = fitted_values
  )
  
  # Create ggplot
  p <- ggplot(df_plot, aes(x = Time)) +
    geom_line(aes(y = Actual, color = "Actual"), size = 1) +
    geom_line(aes(y = Fitted, color = "Fitted"), linetype = "dashed", size = 1) +
    scale_color_manual(values = c("Actual" = "black", "Fitted" = "red")) +
    labs(color = "Legend") +
    ggtitle(paste("ARIMA Fit vs Actual series")) +
    ylab("Cumulative Cases") + 
    xlab("Time") +
    theme_minimal()
  return(list(plot = p, model = model, rmse = rmse, r2 = r2))
}


### Train-test split---------------------
arima_epidemics_split <- function(df, train_ratio=0.8) {
  # order by date
  df <- df[order(df$date), ]
  # get the time series (cum cases)
  series <- ts(df$cumulative_cases, frequency = 52)
  train_size <- floor(train_ratio * length(series))
  train_series <- series[1:train_size]
  test_series <- series[(train_size + 1):length(series)]
  
  # Fit ARIMA model using only the train data
  model <- auto.arima(train_series)
  
  # Get fitted values for the entire time range
  fitted_values <- c(fitted(model), forecast(model, h = length(test_series))$mean)
  
  # Calculate RMSE and R-squared for the test set
  actual_test <- test_series
  fitted_test <- fitted_values[(train_size + 1):length(fitted_values)]
  
  rmse <- sqrt(mean((actual_test - fitted_test)^2))
  r2 <- 1 - sum((actual_test - fitted_test)^2) / sum((actual_test - mean(actual_test))^2)
  
  # Prepare data for ggplot
  df_plot <- data.frame(Time = df$date, Actual = df$cumulative_cases, Fitted = fitted_values)
  
  # Create ggplot
  p <- ggplot(df_plot, aes(x = Time)) +
    geom_line(aes(y = Actual, color = "Actual"), size = 1) +
    geom_line(aes(y = Fitted, color = "Fitted"), linetype = "dashed", size = 1) +
    scale_color_manual(values = c("Actual" = "black", "Fitted" = "red")) +
    labs(color = "Legend") +  # Legend title
    ggtitle(paste("Arima vs Actual")) +
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
  
  return(list(plot = p, model = model, rmse = rmse, r2 = r2))
  }



# 3. Covid---------------------------
## 3.1 Full series---------------
unique_countries <- unique(df_covid$country)[1:10]
evaluate_arima_grid(df_covid, unique_countries)

## 3.2 Train test split-------------------------
evaluate_arima_grid_train(df_covid, unique_countries)

# 4. Epidemics----------------------
# Load and preprocess datasets
df_dengue <- prepare_dataframe(df_dengue)
df_zika <- prepare_dataframe(df_zika)
df_chic <- prepare_dataframe(df_chic)
df_var <- prepare_dataframe(df_var)

## 4.1 Full series----------------------------------
dfs <- list(df_dengue = df_dengue, df_zika = df_zika, df_chic = df_chic, df_var = df_var)

# Apply the function to each dataframe
results <- lapply(names(dfs), function(name) {
  df <- dfs[[name]]
  result <- arima_epidemics(df)
  result$name <- name  # Add the dataframe name to the results
  return(result)
})

# Extract RMSE and R-squared values into a dataframe
results_df <- do.call(rbind, lapply(results, function(res) {
  data.frame(
    Dataset = res$name,
    RMSE = res$rmse,
    R2 = res$r2
  )
}))

# Print the results dataframe
print(results_df)
View(results_df)

# Extract plots
plots <- lapply(results, function(res) res$plot)
library(patchwork)
# Arrange plots in a 2x2 grid using patchwork
combined_plot <- wrap_plots(plots, ncol = 2, nrow = 2)

# Display the combined plot
print(combined_plot)

## 4.2 Train-test split------------------------

# Apply the function to each dataframe
results_split <- lapply(names(dfs), function(name) {
  df <- dfs[[name]]
  result <- arima_epidemics_split(df)
  result$name <- name  # Add the dataframe name to the results
  return(result)
})

# Extract RMSE and R-squared values into a dataframe
results_df_split <- do.call(rbind, lapply(results_split, function(res) {
  data.frame(
    Dataset = res$name,
    RMSE = res$rmse,
    R2 = res$r2
  )
}))

# Print the results dataframe
print(results_df_split)

# Extract plots
plots_split <- lapply(results_split, function(res) res$plot)

# Arrange plots in a 2x2 grid using patchwork
combined_plot_split <- wrap_plots(plots_split, ncol = 2, nrow = 2)

# Display the combined plot
print(combined_plot_split)



