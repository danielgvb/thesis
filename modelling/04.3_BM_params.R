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
df_w <- read.csv("covid_data_weekly.csv")

# 1. Experimets------------------
# weekly
df_col_w <- df_w %>% filter(country == "Colombia")
covid_series_col_w <- df_col_w$new_cases
bm_col <- BM(covid_series_col_w, display = T)
results <- summary(bm_col)
results$coefficients





# 2. Functions---------------------
fit_BM_country <- function(country_name, data, test_prop = 0.20) {
  df_country   <- filter(data, country == country_name)
  covid_series <- df_country$new_cases
  
  # guard clause – skip countries with too-few observations
  if (length(covid_series) < 10L) return(NULL)
  
  split_index  <- floor((1 - test_prop) * length(covid_series))
  train_series <- covid_series[1:split_index]
  
  bm_model     <- BM(train_series, display = FALSE)
  coefs        <- summary(bm_model)$coefficients
  
  # return a named numeric vector (or data.frame – see Option B)
  return(coefs)
}

# 3. Results----------------
coef_df <- map_df(unique_countries,
                  ~{
                    co <- fit_BM_country(.x, df_w, test_split)
                    if (is.null(co)) return(NULL)          # skip if too short
                    tibble(country = .x,
                           term     = names(co),
                           value    = as.numeric(co))
                  })

# wide version (columns per coefficient) if you prefer:
coef_wide <- tidyr::pivot_wider(coef_df, names_from = term, values_from = value)
coef_wide

# 4. To Latex-------------

library(knitr)        # kable()
library(kableExtra)   # extra styling helpers

latex_tbl <- kable(
  coef_wide,                      # your data-frame
  format   = "latex",
  booktabs = TRUE,                # use \toprule, \midrule, \bottomrule
  digits   = 3,                   # round numeric cols
  caption  = "BM coefficients by country",
  label    = "tab:bm_coefs"       # for \ref{tab:bm_coefs}
) %>%
  kable_styling(
    latex_options = c("hold_position", "striped"),
    position = "center"
  )

latex_tbl

library(xtable)

xt <- xtable(
  coef_wide,
  digits  = 3,
  caption = "BM coefficients by country",
  label   = "tab:bm_coefs",
  align   = c("l", rep("r", ncol(coef_wide)))  # l = first col left-align
)

print(
  xt,
  include.rownames = FALSE,
  booktabs         = TRUE,   # needs \usepackage{booktabs}
  file             = "bm_coefficients_by_country.tex"
)

xt

# Epidemics--------------
df_dengue <- read.csv("dengue_no_split.csv")
df_zika<- read.csv("zika.csv")
df_chic <- read.csv("chicunguya.csv")
df_var <-read.csv("varicela.csv")

dengue <- df_dengue$Casos
zika <- df_zika$Casos
chikungunya <- df_chic$Casos
varicella <- df_var$Casos

epidemics <- c(dengue, zika, chikungunya, varicella)

# 2. Functions--------------
fit_BM_epidemics <- function(series, test_prop = 0.20) {
  
  
  
  # guard clause – skip countries with too-few observations
  if (length(series) < 10L) return(NULL)
  
  split_index  <- floor((1 - test_prop) * length(series))
  train_series <- series[1:split_index]
  
  bm_model     <- BM(train_series, display = FALSE)
  coefs        <- summary(bm_model)$coefficients
  
  # return a named numeric vector (or data.frame – see Option B)
  return(coefs)
}

# 3. Results----------------

# wide version (columns per coefficient) if you prefer:



