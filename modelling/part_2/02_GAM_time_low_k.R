rm(list=ls()) # remove everything in memory
# GAM

# SCRIPT CONFIGURATION
# List of required packages
required_packages <- c(
  "dplyr", "stringr", "tidyr", "sf", 
  "spatialsample", "rsample", "mgcv"
)

# Identify missing packages
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

# Install missing packages
if(length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages)
} else {
  cat("All required packages are already installed.\n")
}




# 1. Load all required libraries ------------
# Data manipulation
library(dplyr)
library(stringr)
library(tidyr)

# Spatial analysis
library(sf)
library(spatialsample)
library(rsample)

# Modeling
library(mgcv)


# 2. Define key parameters -------------
# File path for the dataset
FILE_PATH <- "~/GitHub/thesis/Data/platinum/dengue_weather.csv"

# Proportion of departments to use for the final test set
TEST_PROPORTION <- 0.20



# Set a seed for all random processes for reproducibility
set.seed(42)



# 3. DATA LOADING AND PRE-PROCESSING-----------

cat("--- Loading and pre-processing data ---\n")

# Load the raw data
full_df <- read.csv(FILE_PATH)

# Convert the 'week' column from character to Date
full_df$week <- as.Date(full_df$week, format = "%m/%d/%Y")

# create date numeric
full_df$week_num <- as.numeric(full_df$week)



# Create 'departamento' column and convert to a spatial 'sf' object
# The coordinate system (CRS) 4326 is standard for Latitude/Longitude data.
# Create 'departamento' column and convert to a spatial 'sf' object
full_sf_pre <- full_df %>%
  mutate(
    departamento = str_split_i(idx_city, "_", 2)
  ) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

# NEW: Create the t+1 target variable
full_sf <- full_sf_pre %>%
  # Group by each city to perform the shift independently
  group_by(idx_city) %>%
  # Sort by time to ensure the shift is correct
  arrange(week_num) %>%
  # Create the new column 'count_t1' with the value from the next row
  mutate(count_t1 = lead(count, n = 1)) %>%
  # Ungroup to prevent issues later
  ungroup() %>%
  # Remove the last observation for each city, which now has an NA
  drop_na(count_t1)

cat("Data loaded, converted to spatial object, and t+1 target created.\n")




# 4. TRAIN-TEST SPLIT BY DEPARTMENT---------------

# This creates the final hold-out test set, which will not be touched
# during model tuning.

# --- 4. TRAIN-TEST SPLIT BY TIME ---

# This creates the final hold-out test set based on a time cutoff.

cat("\n--- Creating train-test split based on time ---\n")

# Find the cutoff week number that separates the data into train and test sets
# The first (1 - TEST_PROPORTION) of the time period will be for training.
time_range <- range(full_sf$week_num)
cutoff_week <- time_range[1] + (1 - TEST_PROPORTION) * (time_range[2] - time_range[1])

# Create the training and testing sets based on the time cutoff
train_data <- full_sf %>% filter(week_num <= cutoff_week)
test_data  <- full_sf %>% filter(week_num > cutoff_week)

# Report summary of the split
cat("Split complete:\n")
cat("  - Training data:", nrow(train_data), "rows up to week number", round(cutoff_week), "\n")
cat("  - Testing data: ", nrow(test_data), "rows after week number", round(cutoff_week), "\n")

## clean NaNs---------------
library(tidyr)
predictor_cols <- c('count', 'Latitude', 'Longitude', 'week_num',"tavg", "tmax",
                    "prcp", "wdir", "wspd", "pres", "elevation")
test_data_clean <- test_data %>%
  drop_na(all_of(predictor_cols))


# 5. Base Formula--------------------
# base_formula <- as.formula(
#   "count_t1 ~ te(Longitude, Latitude, elevation, k = c(5, 5, 5)) + 
#    s(tmax) + s(prcp, k=5) + s(wspd, k=5) + s(week_num)"
# )

base_formula <- as.formula(
  "count_t1 ~ te(Longitude, Latitude, k=c(5,5))  + s(tmax) +
   s(prcp, k=5)  + s(wspd, k=5)  + s(week_num)"
)


# 5. Fit GAM-Poisson----------------


# Fit the GAM model on the ENTIRE training dataset
model_fit <- gam(base_formula, 
                 family = poisson, 
                 data = train_data, 
                 method = "REML",
                 select = TRUE)

gam.check(model_fit)

summary(model_fit)


plot(model_fit, pages = 4, scheme = 2, scale = 0)

# 6. Fit GAM-Gaussian----------------


model_fit_gauss <- gam(base_formula, 
                 family = gaussian(), 
                 data = train_data, 
                 method = "REML",
                 select = TRUE)

gam.check(model_fit_gauss)

summary(model_fit_gauss)

plot(model_fit_gauss, pages = 2, scheme = 2, scale = 0)

# 7. Compare models---------------
# Assume you have a data frame called 'test_data'
# It must contain the same predictor columns as train_data and the actual 'count' column.

# 1. Get predictions from the Poisson model
predictions_poisson <- predict(model_fit, newdata = test_data_clean, type = "response")

# 2. Get predictions from the Gaussian model
predictions_gauss <- predict(model_fit_gauss, newdata = test_data_clean, type = "response")

# 3. Get the actual values from the test set
# 3. Get the actual values from the test set
actuals <- test_data_clean$count_t1 # <-- FIX: Use the correct target variable

# The rest of the RMSE calculation is now correct
rmse_poisson <- sqrt(mean((predictions_poisson - actuals)^2))
rmse_gauss <- sqrt(mean((predictions_gauss - actuals)^2))
# Print the results
print(paste("Poisson Model RMSE:", round(rmse_poisson, 4)))
print(paste("Gaussian Model RMSE:", round(rmse_gauss, 4)))

# Compare and determine the better model
if (rmse_poisson < rmse_gauss) {
  print("The Poisson model performs better on the test set (lower RMSE).")
} else {
  print("The Gaussian model performs better on the test set (lower RMSE).")
}



# 8. Plots----------------
# Install packages if you don't have them
#install.packages(c("sf", "rnaturalearth", "rnaturalearthdata"))

library(ggplot2)
library(sf)
library(rnaturalearth)

# Get world map and filter for the specific country
country_map <- ne_countries(scale = "large", country = "Colombia", returnclass = "sf")



# Define the number of points for the grid's dimensions
grid_resolution <- 150 # Higher number = finer grid, slower plot

# Create a grid of points covering the bounding box of your data
# Create a grid of points to predict over, based on your data's extent
prediction_grid <- expand_grid(
  Longitude = seq(
    from = st_bbox(train_data)["xmin"],
    to = st_bbox(train_data)["xmax"],
    length.out = 150 # Grid resolution
  ),
  Latitude = seq(
    from = st_bbox(train_data)["ymin"],
    to = st_bbox(train_data)["ymax"],
    length.out = 150
  )
)

# Add the other model predictors to the grid, setting each to its mean
#prediction_grid$elevation <- mean(train_data$elevation)
prediction_grid$tmax      <- mean(train_data$tmax)
prediction_grid$prcp      <- mean(train_data$prcp)
prediction_grid$wspd      <- mean(train_data$wspd)
prediction_grid$week_num  <- round(mean(train_data$week_num)) # Use mean or a typical week

# You can check the first few rows to see the new columns
# head(prediction_grid)


# Predict the effect ONLY for the spatial term
# Predict using your new Gaussian model
spatial_effect <- predict(
  model_fit,               # <-- Use your new model here
  newdata = prediction_grid,
  type = "terms",
  se.fit = TRUE
)

# Combine the grid with the predicted values
prediction_grid$effect <- spatial_effect$fit[, "te(Longitude,Latitude)"]


ggplot() +
  # Plot the predicted effect from your model
  geom_raster(data = prediction_grid, aes(x = Longitude, y = Latitude, fill = effect)) +
  
  # Add the country outline
  geom_sf(data = country_map, fill = NA, color = "black", linewidth = 0.5) +
  
  # Use a nice color scale
  scale_fill_viridis_c(name = "Spatial Effect") +
  
  # Set map coordinates WITHOUT zooming
  coord_sf() + # <-- This is the only change
  
  # Add informative labels
  labs(
    title = "Predicted Spatial Effect (Poisson Model)",
    subtitle = "Effect of Longitude and Latitude on the response",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()


  # 9. Plots 3D-----------------

# Get the map of Colombia
country_map <- ne_countries(scale = "large", country = "Colombia", returnclass = "sf")

# --- Create a new prediction grid for different elevation slices ---

# 1. Define the elevation levels you want to see
# We'll use the 25th, 50th (median), and 75th percentiles from your data
elevation_slices <- quantile(train_data$elevation, probs = c(0.1, 0.2, 0.3, 0.4,
                                                             0.50, 0.6,  0.75, 0.95))

# 2. Create a grid that includes Longitude, Latitude, AND the elevation slices
prediction_grid_3d <- expand_grid(
  Longitude = seq(from = st_bbox(train_data)["xmin"], to = st_bbox(train_data)["xmax"], length.out = 75),
  Latitude = seq(from = st_bbox(train_data)["ymin"], to = st_bbox(train_data)["ymax"], length.out = 75),
  elevation = elevation_slices # Use the specific elevation values
)

# 3. Add the other predictors, setting them to their mean
prediction_grid_3d$tmax     <- mean(train_data$tmax)
prediction_grid_3d$prcp     <- mean(train_data$prcp)
prediction_grid_3d$wspd     <- mean(train_data$wspd)
prediction_grid_3d$week_num <- round(mean(train_data$week_num))

# --- Predict and Plot ---

# 4. Predict the effect using your model (e.g., model_fit_gauss)
spatial_effect_3d <- predict(
  model_fit, # Or model_fit for the Poisson version
  newdata = prediction_grid_3d,
  type = "terms",
  se.fit = TRUE
)

# 5. Combine the grid with the predicted values
# The term name now includes elevation
prediction_grid_3d$effect <- spatial_effect_3d$fit[, "te(Longitude,Latitude,elevation)"]

# 6. Create a label for faceting
prediction_grid_3d$elevation_level <- paste("Elevation:", round(prediction_grid_3d$elevation, 0), "m")


# 7. Create the final plot with facets for each elevation level
ggplot() +
  geom_raster(data = prediction_grid_3d, aes(x = Longitude, y = Latitude, fill = effect)) +
  geom_sf(data = country_map, fill = NA, color = "black", linewidth = 0.5) +
  
  # Use facet_wrap to create a separate map for each elevation slice
  facet_wrap(~ elevation_level) +
  
  scale_fill_viridis_c(name = "Spatio-Elevation\nEffect") +
  coord_sf(
    xlim = st_bbox(train_data)[c("xmin", "xmax")],
    ylim = st_bbox(train_data)[c("ymin", "ymax")]
  ) +
  labs(
    title = "Predicted Spatial Effect by Elevation",
    subtitle = "How the effect of location changes at different altitudes",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

