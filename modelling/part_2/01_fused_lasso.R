# Fused lasso
# import packages

library(genlasso)
install.packages('genlasso')

df <- read.csv("~/GitHub/thesis/Data/gold/dengue_geo_weather.csv")

X <- df[c('Latitude', 'Longitude')]
y <- df[c('count')]


f1 <- fusedlasso(y, X)

