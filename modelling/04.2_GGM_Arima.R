# GGM + ARIMA---------------
# Import packages
library(readxl)
library(DIMORA)
library(dplyr)

# change directory
setwd('../Data/silver/')

# Covid-------------
df_covid <- read.csv("covid_data_weekly.csv")
df_col <- filter(df_covid, country == "Colombia")
cases <- df_col$new_cases

## Toy implementation Full--------------
# GGM
length(cases)
GGM_col <- GGM(cases)
pred_ggm <- predict(GGM_col, newx = c(1:164))
pred_ggm.inst <- make.instantaneous(pred_ggm)
plot(cases, type = "b", xlab = 'Week', ylab = 'Cases')
lines(pred_ggm.inst, lwd=2, col = 3)

#fitted values
fit_ggm <- fitted(GGM_col)
fit_ggm_inst <- make.instantaneous(fit_ggm)

# sarmax refinement
library(forecast)
s2 <- Arima(cumsum(cases), order = c(1,1,1), seasonal = list(order=c(1,1,1), period = 52), xreg = fit_ggm)
summary(s2)
pres2 <- make.instantaneous(fitted(s2))
plot(cases, type = "b", xlab = 'Week', ylab = 'Cases')
lines(pred_ggm.inst, lwd=2, col = 3)
lines(pres2, lty = 1, lwd = 1, col = 2)


## Toy Implementation train test----------

cty <- 'Colombia'
df_country <- filter(df_covid, country == cty)

cases_series <- df_country$new_cases
cases_series


# train-test split
n <- length(cases_series)
n
split_index <- as.integer(length(cases_series) * 0.8)
train <- cases_series[0:split_index]

GGM_train <- GGM(train)

pred_ggm <- predict(GGM_train, newx = c(1:n))
pred_ggm.inst <- make.instantaneous(pred_ggm)



#fitted values full
fit_ggm_full <- predict(GGM_train, newx = c(1:n))
length(fit_ggm_full)

#fitted values train
fit_ggm_train <- fit_ggm_full[1:split_index]
length(fit_ggm_train)


# fitted values test
fit_ggm_test <- fit_ggm_full[split_index:n]
length(fit_ggm_test)

# Sarima refinement  
## 1.  Prepare the training data  (y-series and exogenous regressors)

y_train <- cumsum(cases_series[1:split_index])      # or use the raw series if you prefer
x_train <- fit_ggm_train                            # matrix / data-frame of regressors

## If you need the seasonal period to be 52 weeks, be sure the
## series knows that:
y_train <- ts(y_train, frequency = 52)


## 2.  Fit an automatic ARIMA with external regressors

s2 <- auto.arima(
  y_train,
  xreg       = x_train,
  seasonal   = TRUE,       # let it decide (P,D,Q); keeps period = 52
  stepwise   = FALSE,      # ↑↑  a bit slower but more thorough search
  approximation = FALSE   # ↑↑  forces exact likelihood
)

summary(s2)


## 3.  Forecast on the hold-out period

h     <- n - split_index              # number of steps ahead
new_x <- fit_ggm_test                 # exogenous regressors for the horizon

fc <- forecast(s2, xreg = new_x, h = h)


## 4.  Collect fitted values and point forecasts

fitted_arima   <- fitted(s2)
forecast_arima <- fc$mean

full_model_fitted <- c(fitted_arima, forecast_arima)

#plot(cumsum(cases_series), type = 'b')
#lines(full_model_fitted, lwd = 2, col = 3)

# save the results in a list
#fitted_list[[cty]] <- full_model_fitted



plot(cumsum(cases), type = 'b')
lines(full_model_fitted, lwd = 2, col = 3)


# Covid Loop-------------------
unique_countries <- unique(df_covid$country)[1:10]
unique_countries

fitted_list <- vector("list", length(unique_countries ))
names(fitted_list) <- unique_countries  

for (cty in unique_countries){
  print(cty)
  # define series:
  df_country <- filter(df_covid, country == cty)
  cases_series <- df_country$new_cases
  cases_series
  
  # train-test split
  n <- length(cases_series)
  split_index <- as.integer(length(cases_series) * 0.8)
  train <- cases_series[0:split_index]
  
  GGM_train <- GGM(train)
  
  pred_ggm <- predict(GGM_train, newx = c(1:n))
  pred_ggm.inst <- make.instantaneous(pred_ggm)
  
  
  
  #fitted values full
  fit_ggm_full <- predict(GGM_train, newx = c(1:n))
  length(fit_ggm_full)
  
  #fitted values train
  fit_ggm_train <- fit_ggm_full[1:split_index]
  length(fit_ggm_train)
  
  
  # fitted values test
  fit_ggm_test <- fit_ggm_full[(split_index+1):n]
  length(fit_ggm_test)
  
  # Sarima refinement  
  ## 1.  Prepare the training data  (y-series and exogenous regressors)
  
  y_train <- cumsum(cases_series[1:split_index])      # or use the raw series if you prefer
  x_train <- fit_ggm_train                            # matrix / data-frame of regressors
  
  ## If you need the seasonal period to be 52 weeks, be sure the
  ## series knows that:
  y_train <- ts(y_train, frequency = 52)
  
  
  ## 2.  Fit an automatic ARIMA with external regressors
  
  s2 <- auto.arima(
    y_train,
    xreg       = x_train,
    seasonal   = TRUE,       # let it decide (P,D,Q); keeps period = 52
    stepwise   = FALSE,      # ↑↑  a bit slower but more thorough search
    approximation = FALSE   # ↑↑  forces exact likelihood
  )
  
  summary(s2)
  
  
  ## 3.  Forecast on the hold-out period
  
  h     <- n - split_index              # number of steps ahead
  new_x <- fit_ggm_test                 # exogenous regressors for the horizon
  
  fc <- forecast(s2, xreg = new_x, h = h)
  
  
  ## 4.  Collect fitted values and point forecasts
  
  fitted_arima   <- fitted(s2)
  forecast_arima <- fc$mean
  
  full_model_fitted <- c(fitted_arima, forecast_arima)
  
  #plot(cumsum(cases_series), type = 'b')
  #lines(full_model_fitted, lwd = 2, col = 3)
  
  # save the results in a list
  fitted_list[[cty]] <- full_model_fitted
}


# combine results
# Bind to a data-frame  (columns = countries, rows = weeks)
fitted_df <- as.data.frame(do.call(cbind, fitted_list))

View(fitted_df)

# set the date column 
fitted_df$date <- df_country$date

# save to dir

# save the wide table
file_path <- "../../results/GGM-ARIMA_covid.csv"
dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
write.csv(fitted_df, file_path, row.names = FALSE)


# Epidemics -------------------------
## Import data-------------
df_dengue <- read.csv("dengue_no_split.csv")
df_zika<- read.csv("zika.csv")
df_chic <- read.csv("chicunguya.csv")
df_var <-read.csv("varicela.csv")

## Dengue --------------------

cases_series <- df_dengue$Casos

# train-test split
n <- length(cases_series)
split_index <- as.integer(length(cases_series) * 0.8)
train <- cases_series[0:split_index]

GGM_train <- GGM(train)


#fitted values full
fit_ggm_full <- predict(GGM_train, newx = c(1:n))
length(fit_ggm_full)

#fitted values train
fit_ggm_train <- fit_ggm_full[1:split_index]
length(fit_ggm_train)


# fitted values test
fit_ggm_test <- fit_ggm_full[(split_index+1):n]
length(fit_ggm_test)

# Sarima refinement  
## 1.  Prepare the training data  (y-series and exogenous regressors)

y_train <- cumsum(cases_series[1:split_index])      # or use the raw series if you prefer
x_train <- fit_ggm_train                            # matrix / data-frame of regressors

y_train <- ts(y_train, frequency = 52)


## 2.  Fit an automatic ARIMA with external regressors

s2 <- auto.arima(
  y_train,
  xreg       = x_train,
  seasonal   = TRUE,       # let it decide (P,D,Q); keeps period = 52
  stepwise   = FALSE,      # ↑↑  a bit slower but more thorough search / FALSE is too slow
  approximation = TRUE   # ↑↑  forces exact likelihood / FALSE is too slow
)

summary(s2)


## 3.  Forecast on the hold-out period

h     <- n - split_index              # number of steps ahead
new_x <- fit_ggm_test                 # exogenous regressors for the horizon

fc <- forecast(s2, xreg = new_x, h = h)


## 4.  Collect fitted values and point forecasts

fitted_arima   <- fitted(s2)
forecast_arima <- fc$mean

full_model_fitted_dengue <- c(fitted_arima, forecast_arima)

plot(cumsum(cases_series), type = 'b')
lines(full_model_fitted_dengue, lwd = 2, col = 3)

df_dengue$fitted <- full_model_fitted_dengue

## Zika-----------

cases_series <- df_zika$Casos

# train-test split
n <- length(cases_series)
split_index <- as.integer(length(cases_series) * 0.8)
train <- cases_series[0:split_index]

GGM_train <- GGM(train)


#fitted values full
fit_ggm_full <- predict(GGM_train, newx = c(1:n))
length(fit_ggm_full)

#fitted values train
fit_ggm_train <- fit_ggm_full[1:split_index]
length(fit_ggm_train)


# fitted values test
fit_ggm_test <- fit_ggm_full[(split_index+1):n]
length(fit_ggm_test)

# Sarima refinement  
## 1.  Prepare the training data  (y-series and exogenous regressors)

y_train <- cumsum(cases_series[1:split_index])      # or use the raw series if you prefer
x_train <- fit_ggm_train                            # matrix / data-frame of regressors

y_train <- ts(y_train, frequency = 52)


## 2.  Fit an automatic ARIMA with external regressors

s2 <- auto.arima(
  y_train,
  xreg       = x_train,
  seasonal   = TRUE,       # let it decide (P,D,Q); keeps period = 52
  stepwise   = FALSE,      # ↑↑  a bit slower but more thorough search
  approximation = TRUE   # ↑↑  forces exact likelihood
)

summary(s2)


## 3.  Forecast on the hold-out period

h     <- n - split_index              # number of steps ahead
new_x <- fit_ggm_test                 # exogenous regressors for the horizon

fc <- forecast(s2, xreg = new_x, h = h)


## 4.  Collect fitted values and point forecasts

fitted_arima   <- fitted(s2)
forecast_arima <- fc$mean

full_model_fitted_zika <- c(fitted_arima, forecast_arima)

plot(cumsum(cases_series), type = 'b')
lines(full_model_fitted_zika, lwd = 2, col = 3)

df_zika$fitted <- full_model_fitted_zika

## Chikungunya--------------

cases_series <- df_chic$Casos

# train-test split
n <- length(cases_series)
split_index <- as.integer(length(cases_series) * 0.8)
train <- cases_series[0:split_index]

GGM_train <- GGM(train)


#fitted values full
fit_ggm_full <- predict(GGM_train, newx = c(1:n))
length(fit_ggm_full)

#fitted values train
fit_ggm_train <- fit_ggm_full[1:split_index]
length(fit_ggm_train)


# fitted values test
fit_ggm_test <- fit_ggm_full[(split_index+1):n]
length(fit_ggm_test)

# Sarima refinement  
## 1.  Prepare the training data  (y-series and exogenous regressors)

y_train <- cumsum(cases_series[1:split_index])      # or use the raw series if you prefer
x_train <- fit_ggm_train                            # matrix / data-frame of regressors

y_train <- ts(y_train, frequency = 52)


## 2.  Fit an automatic ARIMA with external regressors

s2 <- auto.arima(
  y_train,
  xreg       = x_train,
  seasonal   = TRUE,       # let it decide (P,D,Q); keeps period = 52
  stepwise   = TRUE,      # ↑↑  a bit slower but more thorough search
  approximation = TRUE   # ↑↑  forces exact likelihood
)

summary(s2)


## 3.  Forecast on the hold-out period

h     <- n - split_index              # number of steps ahead
new_x <- fit_ggm_test                 # exogenous regressors for the horizon

fc <- forecast(s2, xreg = new_x, h = h)


## 4.  Collect fitted values and point forecasts

fitted_arima   <- fitted(s2)
forecast_arima <- fc$mean

full_model_fitted_chic <- c(fitted_arima, forecast_arima)

plot(cumsum(cases_series), type = 'b')
lines(full_model_fitted_chic, lwd = 2, col = 3)

df_chic$fitted <- full_model_fitted_chic

## Varicela----------
cases_series <- df_var$Casos

# train-test split
n <- length(cases_series)
split_index <- as.integer(length(cases_series) * 0.8)
train <- cases_series[0:split_index]

GGM_train <- GGM(train)


#fitted values full
fit_ggm_full <- predict(GGM_train, newx = c(1:n))
length(fit_ggm_full)

#fitted values train
fit_ggm_train <- fit_ggm_full[1:split_index]
length(fit_ggm_train)


# fitted values test
fit_ggm_test <- fit_ggm_full[(split_index+1):n]
length(fit_ggm_test)

# Sarima refinement  
## 1.  Prepare the training data  (y-series and exogenous regressors)

y_train <- cumsum(cases_series[1:split_index])      # or use the raw series if you prefer
x_train <- fit_ggm_train                            # matrix / data-frame of regressors

y_train <- ts(y_train, frequency = 52)


## 2.  Fit an automatic ARIMA with external regressors

s2 <- auto.arima(
  y_train,
  xreg       = x_train,
  seasonal   = TRUE,       # let it decide (P,D,Q); keeps period = 52
  stepwise   = TRUE,      # ↑↑  a bit slower but more thorough search
  approximation = TRUE   # ↑↑  forces exact likelihood
)

summary(s2)


## 3.  Forecast on the hold-out period

h     <- n - split_index              # number of steps ahead
new_x <- fit_ggm_test                 # exogenous regressors for the horizon

fc <- forecast(s2, xreg = new_x, h = h)


## 4.  Collect fitted values and point forecasts

fitted_arima   <- fitted(s2)
forecast_arima <- fc$mean

full_model_fitted_var <- c(fitted_arima, forecast_arima)

plot(cumsum(cases_series), type = 'b')
lines(full_model_fitted_var, lwd = 2, col = 3)

df_var$fitted <- full_model_fitted_var

## Join Data----------------
# set simpler dataframes
library(dplyr)

df_dengue_s <- df_dengue %>% 
  transmute(DATE, Dengue = fitted)          # keeps only these two columns

df_zika_s   <- df_zika   %>% 
  transmute(DATE, Zika = fitted)

df_chic_s   <- df_chic   %>% 
  transmute(DATE, Chikungunya = fitted)

df_var_s    <- df_var    %>% 
  transmute(DATE, Varicella = fitted)



# merge
df_merge <- merge(x = df_dengue_s, y = df_zika_s, by = "DATE", all = TRUE)
df_merge2 <- merge(x = df_merge, y = df_var_s, by = "DATE", all = TRUE)
df_merge_final <- merge(x = df_merge2, y = df_chic_s, by = "DATE", all = TRUE)

View(df_merge_final)

## Save ------------
# save the wide table
file_path <- "../../results/GGM-ARIMA_epidemics.csv"
dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
write.csv(df_merge_final, file_path, row.names = FALSE)

