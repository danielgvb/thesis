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
  "count_t1 ~ s(count) + te(Longitude, Latitude, k=c(12,12))  + s(tmax) +
   s(prcp, k=5)  + s(wspd, k=5)  + s(week_num, k=5)"
)

# k de te(lat,lon) = 10, 15, 17 para el gaussian da muy bueno


# 6. Fit GAM-Poisson ----------------
model_fit_poisson <- gam(base_formula,
                         family = poisson,
                         data = train_data,
                         method = "REML",
                         select = TRUE)

# 7. Fit GAM-Gaussian ----------------
model_fit_gauss <- gam(base_formula,
                       family = gaussian(),
                       data = train_data,
                       method = "REML",
                       select = TRUE)

# 8. Fit GAM-QuasiPoisson -----------------
model_fit_quasi <- gam(base_formula,
                       family = quasipoisson,
                       data = train_data,
                       method = "REML",
                       select = TRUE)

# 9. Fit GAM-Log-Normal -----------------

model_fit_lognormal <- gam(base_formula,
                           family = gaussian(link = "log"), # Use gaussian family with a log link
                           data = train_data,
                           method = "REML",
                           select = TRUE)

gam.check(model_fit_lognormal)
summary(model_fit_lognormal)


# 10. Compare all four models ---------------

# 1. Get predictions from all models
predictions_poisson <- predict(model_fit_poisson, newdata = test_data_clean, type = "response")
predictions_gauss <- predict(model_fit_gauss, newdata = test_data_clean, type = "response")
predictions_quasi <- predict(model_fit_quasi, newdata = test_data_clean, type = "response")
predictions_lognormal <- predict(model_fit_lognormal, newdata = test_data_clean, type = "response") # New

# 2. Get the actual values from the test set
actuals <- test_data_clean$count_t1

# 3. Calculate RMSE for each model
rmse_poisson <- sqrt(mean((predictions_poisson - actuals)^2))
rmse_gauss <- sqrt(mean((predictions_gauss - actuals)^2))
rmse_quasi <- sqrt(mean((predictions_quasi - actuals)^2))
rmse_lognormal <- sqrt(mean((predictions_lognormal - actuals)^2)) # New

# 4. Print the results
print(paste("Poisson Model RMSE:", round(rmse_poisson, 4)))
print(paste("Gaussian Model RMSE:", round(rmse_gauss, 4)))
print(paste("Quasipoisson Model RMSE:", round(rmse_quasi, 4)))
print(paste("Log-Normal Model RMSE:", round(rmse_lognormal, 4))) # New

# 5. Programmatically find and announce the best model
rmse_values <- c(Poisson = rmse_poisson,
                 Gaussian = rmse_gauss,
                 Quasipoisson = rmse_quasi,
                 `Log-Normal` = rmse_lognormal) # Added new model to comparison

best_model_name <- names(which.min(rmse_values))
min_rmse <- min(rmse_values)

cat("\n--- Comparison Result ---\n")
print(paste0("The ", best_model_name, " model performs best on the test set with an RMSE of ", round(min_rmse, 4), "."))

# ¨Poisson is best with 7.1319 RMSE

# 11. Best smoother-----------------
## 11.1. Define Variables and Bases to Test ---------

# List of predictor variables that have a smooth term
# Note: We are not tuning the 'te' term for simplicity, but you could adapt the script to do so.
vars_to_test <- c("count", "tmax", "prcp", "wspd", "week_num")

# A list of common basis types to try for each variable.
# 'tp' = Thin Plate Regression Splines (default)
# 'ts' = Thin Plate with shrinkage
# 'cr' = Cubic Regression Splines
# 'ps' = P-splines
# 'cc' = Cyclic Cubic (ideal for cyclical predictors like week_num)
bases_to_test <- c("tp", "ts", "cr", "ps")
cyclic_bases_to_test <- c("cc", "ps", "cr") # A specific set for the week_num variable

## 11.2. Iterative Search for Best Basis ----------

# This will store the best basis found for each variable
best_bases <- list(
  count = "s(count, k=-1, bs='tp')", # Start with defaults
  tmax = "s(tmax, k=-1, bs='tp')",
  prcp = "s(prcp, k=5, bs='tp')",
  wspd = "s(wspd, k=5, bs='tp')",
  week_num = "s(week_num, k=5, bs='tp')"
)

# The fixed part of our formula
fixed_formula_part <- "te(Longitude, Latitude, k=c(12,12))"

cat("Starting the search for the best smoothing basis for each variable...\n\n")

# Loop through each variable to test
for (variable in vars_to_test) {
  
  cat(paste0("--- Testing variable: '", variable, "' ---\n"))
  
  # Store results (AIC for each basis type) for the current variable
  results <- data.frame(variable = character(), basis = character(), aic = numeric())
  
  # Determine which set of bases to test
  current_bases_to_test <- if (variable == "week_num") cyclic_bases_to_test else bases_to_test
  
  # Loop through each basis type for the current variable
  for (basis in current_bases_to_test) {
    
    # Construct the smooth term for the current variable and basis
    # We'll use the k value from your original formula, or -1 for gam to choose
    k_val <- switch(variable,
                    "prcp" = 5,
                    "wspd" = 5,
                    "week_num" = 5,
                    -1) # Default k for count, tmax
    
    current_smooth <- paste0("s(", variable, ", k=", k_val, ", bs='", basis, "')")
    
    # Build the full formula for this iteration
    # It includes the fixed part, the smooth we are currently testing,
    # and the best smooths found so far for the other variables.
    other_vars <- setdiff(vars_to_test, variable)
    other_smooths <- sapply(other_vars, function(v) best_bases[[v]])
    
    formula_str <- paste("count_t1 ~", current_smooth, "+",
                         paste(other_smooths, collapse = " + "), "+",
                         fixed_formula_part)
    
    formula_obj <- as.formula(formula_str)
    
    # Fit the GAM
    model_fit <- gam(formula_obj,
                     family = gaussian(),
                     data = train_data,
                     method = "REML",
                     select = TRUE)
    
    # Store the result
    current_aic <- AIC(model_fit)
    results <- rbind(results, data.frame(variable = variable, basis = basis, aic = current_aic))
    
    cat(paste0("  Basis '", basis, "' -> AIC: ", round(current_aic, 2), "\n"))
  }
  
  # Find the best basis for the current variable (the one with the minimum AIC)
  best_result <- results[which.min(results$aic), ]
  
  # Update our list of best bases with the winner for this variable
  best_k_val <- switch(best_result$variable,
                       "prcp" = 5,
                       "wspd" = 5,
                       "week_num" = 5,
                       -1)
  best_bases[[variable]] <- paste0("s(", best_result$variable, ", k=", best_k_val, ", bs='", best_result$basis, "')")
  
  cat(paste0("\n  >> Best basis for '", variable, "' is '", best_result$basis, "'\n\n"))
}


## 11.3. Construct Final Model ---------------

# Now that we have the best basis for each variable, let's build the final formula
final_smooth_terms <- paste(unlist(best_bases), collapse = " + ")
final_formula_str <- paste("count_t1 ~", final_smooth_terms, "+", fixed_formula_part)
final_formula <- as.formula(final_formula_str)

cat("------------------------------------------\n")
cat("Final optimized formula:\n")
print(final_formula)
cat("------------------------------------------\n\n")
#gam_gaus_formula< - count_t1 ~ s(count, k = -1, bs = "ps") + s(tmax, k = -1, bs = "cr") + 
#  s(prcp, k = 5, bs = "cr") + s(wspd, k = 5, bs = "tp") + s(week_num, 
#                                                            k = 5, bs = "ps") + te(Longitude, Latitude, k = c(12, 12))

# Fit the final, optimized model
final_model <- gam(final_formula,
                   family = gaussian(), # change for best model
                   data = train_data,
                   method = "REML",
                   select = TRUE)

# View the summary of the final model
cat("Summary of the final model:\n")
summary(final_model)



# 12. Plots----------------

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
prediction_grid$count      <- mean(train_data$count) # comment 
prediction_grid$tmax      <- mean(train_data$tmax)
prediction_grid$prcp      <- mean(train_data$prcp)
prediction_grid$wspd      <- mean(train_data$wspd)
prediction_grid$week_num  <- round(mean(train_data$week_num)) # Use mean or a typical week

# You can check the first few rows to see the new columns
# head(prediction_grid)


# Predict the effect ONLY for the spatial term
# Predict using your new Gaussian model
spatial_effect <- predict(
  model_fit_gauss,               # <-- Use your new model here
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

# 13. Clipped Plots------------
# 2. Get Country Map
country_map <- ne_countries(scale = "large", country = "Colombia", returnclass = "sf")

# 3. Create Prediction Grid
# This part is the same as your original code
prediction_grid <- expand_grid(
  Longitude = seq(
    from = st_bbox(train_data)["xmin"],
    to = st_bbox(train_data)["xmax"],
    length.out = 150
  ),
  Latitude = seq(
    from = st_bbox(train_data)["ymin"],
    to = st_bbox(train_data)["ymax"],
    length.out = 150
  )
)

# 4. Clip the Grid to the Country Borders [KEY CHANGE]
# Convert the grid to a spatial object (sf) and then clip it
clipped_grid <- prediction_grid %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(country_map)) %>%
  st_intersection(country_map)

# The 'clipped_grid' now only contains points inside Colombia.
# We need to extract the coordinates back into Longitude/Latitude columns for ggplot
clipped_coords <- st_coordinates(clipped_grid)
clipped_grid$Longitude <- clipped_coords[, "X"]
clipped_grid$Latitude <- clipped_coords[, "Y"]


# 5. Add Predictors and Make Predictions
# Add other predictors to the clipped grid
# clipped_grid$elevation <- mean(train_data$elevation)
clipped_grid$count     <- mean(train_data$count)
clipped_grid$tmax      <- mean(train_data$tmax)
clipped_grid$prcp      <- mean(train_data$prcp)
clipped_grid$wspd      <- mean(train_data$wspd)
clipped_grid$week_num  <- round(mean(train_data$week_num))

# Predict using the clipped grid
spatial_effect <- predict(
  final_model,
  newdata = clipped_grid,
  type = "terms",
  se.fit = TRUE
)

# Combine the clipped grid with the predicted values
clipped_grid$effect <- spatial_effect$fit[, "te(Longitude,Latitude)"]


# 6. Plot the Clipped Spatial Effect
ggplot() +
  # Plot the predicted effect using the CLIPPED data
  geom_raster(data = clipped_grid, aes(x = Longitude, y = Latitude, fill = effect)) +
  
  # Add the country outline
  geom_sf(data = country_map, fill = NA, color = "black", linewidth = 0.5) +
  
  # Use a nice color scale
  scale_fill_viridis_c(name = "Spatial Effect") +
  
  # Set map coordinates
  coord_sf(crs = st_crs(country_map)) +
  
  # Add informative labels
  labs(
    title = "Predicted Spatial Effect (Clipped to Borders)",
    subtitle = "Effect of Longitude and Latitude on the response",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

# 14. Plots 3D-----------------

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

