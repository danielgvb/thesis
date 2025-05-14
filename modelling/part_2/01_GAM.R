# GAM
# import packages
library(dplyr)
library(gam)



df <- read.csv("~/GitHub/thesis/Data/platinum/dengue_weather.csv")

X <- df[c('Latitude', 'Longitude', "tavg", "tmin", "tmax", "prcp", "wdir", "wspd", "pres", "elevation")]
y <- df[c('count')]

g1 <- gam(count~s(Latitude)+s(Longitude)  + s(elevation)+ s(tavg) + s(tmax) + s(prcp) + 
            s(wdir) + s(wspd) + s(pres), family = poisson, data = df)
summary(g1)
# Latitude has parametric effect, Longitude no (so north south has effect: might be bc of the caribean and amazon)
# Latitude is north south / longitude is wast west
# all have non-parametric effect
# Non parametric are the smoothing terms, coefficients of the basis fn -> check non linear relat/
# Parametric are the regular betas of the OLS -> check for linear relat/

g2 <- gam(count~lo(Latitude)+lo(Longitude)  + s(elevation)+ s(tavg) + s(tmax) + s(prcp) + 
            s(wdir) + s(wspd) + s(pres), family = poisson, data = df)
summary(g2)
AIC(g1)

par(mfrow=c(2,2))
plot(g2)
AIC(g2)

#AIC g2 < AIC g1