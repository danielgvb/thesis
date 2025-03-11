
# load packages
library(readxl)
library(DIMORA)
library(dplyr)
# change directory
setwd('Github/pandemic_modelling/Data/silver/')

# monthly data------------------
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

# weekly data------------------------
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



# daily data------------------------
df_d<- read.csv("covid_data.csv")

# filter dataframe
df_col_d <- df_d %>% filter(country == "Colombia")
covid_series_col_d <- df_col_d$new_cases
# run simple bass model
bm_d <-BM(covid_series_col_d,display = T) # show graphical view of results / display = True
summary(bm_d)

# do for 25% of data
covid_series_col_ds <- covid_series_col_w[1:75]
bm_d <-BM(covid_series_col_ds,display = T) # show graphical view of results / display = True
