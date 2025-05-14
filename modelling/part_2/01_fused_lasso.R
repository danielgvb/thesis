# Fused lasso
# import packages
library(dplyr)



df <- read.csv("~/GitHub/thesis/Data/platinum/dengue_weather.csv")

X <- df[c('Latitude', 'Longitude', "tavg", "tmin", "tmax", "prcp", "wdir", "wspd", "pres")]
y <- df[c('count')]


