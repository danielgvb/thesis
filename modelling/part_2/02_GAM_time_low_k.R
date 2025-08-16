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

base_formula <- as.formula("count_t1 ~  te(Longitude, Latitude)  + s(tmax) +
   s(prcp)  + s(wspd)  + s(week_num)"
)


base_formula_2 <- as.formula("count_t1 ~s(count) + te(Longitude, Latitude)  + s(tmax) +
   s(prcp)  + s(wspd)  + s(week_num)"
)


# k de te(lat,lon) = 10, 15, 17 para el gaussian da muy bueno


# 6. Fit GAM-Poisson ----------------
model_fit_poisson <- gam(base_formula,
                         family = poisson,
                         data = train_data,
                         method = "REML",
                         select = TRUE)
plot(model_fit_poisson, pages = 1, scheme = 2, scale = 0)

# 7. Fit GAM-Gaussian ----------------
model_fit_gauss <- gam(base_formula,
                       family = gaussian(),
                       data = train_data,
                       method = "REML",
                       select = TRUE)
plot(model_fit_gauss, pages = 1, scheme = 2, scale = 0)
# gauss is way smoother

# 8. Fit GAM-QuasiPoisson -----------------
model_fit_quasi <- gam(base_formula,
                       family = quasipoisson,
                       data = train_data,
                       method = "REML",
                       select = TRUE)
plot(model_fit_quasi, pages = 1, scheme = 2, scale = 0)

# 9. Fit GAM-Log-Normal -----------------

model_fit_lognormal <- gam(base_formula,
                           family = gaussian(link = "log"), # Use gaussian family with a log link
                           data = train_data,
                           method = "REML",
                           select = TRUE)
plot(model_fit_lognormal, pages = 1, scheme = 2, scale = 0)



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

# ¨quasiPoisson is best with 7.1319 RMSE

## 10.1 Print Table---------------
results_df <- data.frame(
  Model = c("Poisson", "Gaussian", "Quasipoisson", "Log-Normal"),
  RMSE = c(rmse_poisson, rmse_gauss, rmse_quasi, rmse_lognormal)
)

# Sort the data frame by RMSE for better presentation
results_df <- results_df[order(results_df$RMSE), ]


# 2. Load the xtable library
library(xtable)

# 3. Create the LaTeX table object
# We specify a caption, a label for cross-referencing, and the number of digits
latex_table <- xtable(results_df,
                      caption = "Comparison of Root Mean Square Error (RMSE) for GAM Models",
                      label = "tab:gam_rmse_comparison",
                      digits = 4)

# 4. Print the LaTeX code to the console
# include.rownames = FALSE cleans up the output
# comment = FALSE removes the xtable timestamp
print(latex_table,
      include.rownames = FALSE,
      comment = FALSE)

# 11. Best smoother-----------------
## 11.1. Setup: Define variables and bases from  formula ----------------

base_formula <- as.formula("count_t1 ~ te(Longitude, Latitude) + s(tmax) + s(prcp) + s(wspd) + s(week_num)")




# Automatically get the predictor variables with smooth terms from the formula
# This avoids manually listing them and accidentally including the response variable
formula_terms <- attributes(terms(base_formula))$term.labels
smooth_vars <- formula_terms[grepl("^s\\(", formula_terms)]
vars_to_test <- gsub("s\\(([^,)]+).*", "\\1", smooth_vars)

# The fixed part of our formula (spatial term)
fixed_formula_part <- "te(Longitude, Latitude)"

# A list of common basis types to try for each variable
bases_to_test <- c("tp", "ts", "cr", "ps")
# A specific set for cyclical variables like week_num
cyclic_bases_to_test <- c("cc", "ps", "tp")

cat("Identified variables to test:", paste(vars_to_test, collapse = ", "), "\n\n")


## 11.2. Iterative Search for Best Basis ------------------

# Initialize a list to store the best basis found for each variable.
# We start with the default 'tp' basis for all.
best_bases <- setNames(
  paste0("s(", vars_to_test, ", bs='tp')"),
  vars_to_test
)

cat("Starting the search for the best smoothing basis for each variable...\n\n")

# Loop through each variable to test
for (variable in vars_to_test) {
  
  cat(paste0("--- Testing variable: '", variable, "' ---\n"))
  
  # Store results (AIC for each basis type) for the current variable
  results <- data.frame(variable = character(), basis = character(), aic = numeric())
  
  # Determine which set of bases to test for the current variable
  current_bases_to_test <- if (variable == "week_num") cyclic_bases_to_test else bases_to_test
  
  # Loop through each basis type
  for (basis in current_bases_to_test) {
    
    # Construct the smooth term for the current variable and basis.
    # We are NOT setting 'k', allowing gam() to choose the default.
    current_smooth <- paste0("s(", variable, ", bs='", basis, "')")
    
    # Get the best smooths found so far for the *other* variables.
    other_vars <- setdiff(vars_to_test, variable)
    other_smooths <- best_bases[other_vars]
    
    # Build the full formula for this iteration
    formula_str <- paste("count_t1 ~",
                         current_smooth, "+",
                         paste(other_smooths, collapse = " + "), "+",
                         fixed_formula_part)
    
    formula_obj <- as.formula(formula_str)
    
    # Fit the GAM. Using gaussian() as in your original script.
    # select = TRUE is important as it helps regularize and remove unneeded complexity.
    model_fit <- gam(formula_obj,
                     family = gaussian(), # Change family if needed (e.g., poisson())
                     data = train_data,
                     method = "REML",
                     select = TRUE)
    
    # Store the AIC
    current_aic <- AIC(model_fit)
    results <- rbind(results, data.frame(variable = variable, basis = basis, aic = current_aic))
    
    cat(paste0("  Basis '", basis, "' -> AIC: ", round(current_aic, 2), "\n"))
  }
  
  # Find the best basis for the current variable (the one with the minimum AIC)
  best_result <- results[which.min(results$aic), ]
  
  # Update our list of best bases with the winner for this variable
  best_bases[[variable]] <- paste0("s(", best_result$variable, ", bs='", best_result$basis, "')")
  
  cat(paste0("\n  >> Best basis for '", variable, "' is '", best_result$basis, "'\n\n"))
}


## 11.3. Final Output -------------------

# Combine all the best parts into the final formula string
final_formula_str <- paste("count_t1 ~",
                           paste(best_bases, collapse = " + "), "+",
                           fixed_formula_part)

final_formula <- as.formula(final_formula_str)

cat("--- Search Complete ---\n")
cat("The final, optimized formula is:\n")
print(final_formula)

final_formula <- as.formula("count_t1 ~ s(tmax, bs = 'tp', k=6) + 
                                       s(prcp, bs = 'ts') + 
                                       s(wspd, bs = 'ps') + 
                                       s(week_num, bs = 'ps') + 
                                       te(Longitude, Latitude)")


final_formula <- as.formula("count_t1 ~ s(tmax, bs = 'tp', k=6) + 
                                       s(prcp, bs = 'ts') + 
                                       s(wspd, bs = 'ps') + 
                                       s(week_num, bs = 'ps') + 
                                       te(Longitude, Latitude)+
                                        s(count)")




# Fit the final, optimized model
final_model <- gam(final_formula,
                   family = gaussian(), # change for best model
                   data = train_data,
                   method = "REML",
                   select = TRUE)


plot(final_model, pages = 1, scheme = 2, scale = 0)

# View the summary of the final model
cat("Summary of the final model:\n")
summary(final_model)

## 11.4 Latex Table print---------------
library(xtable)

# --- 1. Get the model summary and extract the necessary parts ---

# Get the summary object once to avoid re-computing
model_summary <- summary(final_model)

# Extract the smooth terms table
smooth_table <- model_summary$s.table

# Extract the deviance explained and format it as a percentage string for LaTeX
# Note: model_summary$dev.expl is a proportion, so we multiply by 100
dev_explained_pct <- paste0(round(model_summary$dev.expl * 100, 2), "\\%")


# --- 2. Create the xtable object ---

xtable_smooth <- xtable(smooth_table, 
                        caption = "Approximate Significance of Smooth Terms and Model Fit",
                        label = "tab:gam_smooth_terms_fit",
                        digits = c(0, 4, 2, 2, 4)) # Digits for each column


# --- 3. Define the custom row to add ---

# This list tells print.xtable where to add a command and what command to add.
# 'pos' is a list of positions. nrow(smooth_table) means "after the last data row".
# 'command' is the LaTeX string to insert.
# We add a \hline, then a row that spans 5 columns (\multicolumn) for our text.
add_row <- list()
add_row$pos <- list(nrow(smooth_table))
add_row$command <- paste0("\\hline \n",
                          "\\multicolumn{5}{r}{Deviance Explained: ", dev_explained_pct, "} \\\\ \n")


# --- 4. Print the final LaTeX table ---

# The add.to.row argument inserts our custom string at the specified position
print(xtable_smooth, 
      comment = FALSE,
      add.to.row = add_row)


# --- Table 2: Parametric Coefficients (Intercept) ---

# Extract the parametric coefficients table
parametric_table <- summary(final_model)$p.table

# Create a LaTeX table from it
xtable_parametric <- xtable(parametric_table,
                            caption = "Parametric Coefficients of the GAM",
                            label = "tab:gam_parametric_terms",
                            digits = c(0, 4, 4, 2, 4))

# Print the LaTeX code to the console
print(xtable_parametric, comment = FALSE)


gam.check(final_model)
# Plot just the spatial term as a contour map
plot(final_model, pages=5, scheme = 2)


# 12. Plots----------------

library(ggplot2)
library(sf)
library(rnaturalearth)

#install.packages("devtools")

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


# hear colors-------------
ggplot() +
  # Plot the predicted effect using the CLIPPED data
  geom_raster(data = clipped_grid, aes(x = Longitude, y = Latitude, fill = effect)) +
  
  # Add the country outline
  geom_sf(data = country_map, fill = NA, color = "black", linewidth = 0.5) +
  
  # Use a Red-to-Blue color scale
  scale_fill_distiller(palette = "RdBu", name = "Spatial Effect") +
  
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

# 14. Count vs no count-----------------
# Formula without the autoregressive term
formula_base <- as.formula("count_t1 ~ s(tmax, bs = 'tp', k=6) + 
                                       s(prcp, bs = 'ts') + 
                                       s(wspd, bs = 'ps') + 
                                       s(week_num, bs = 'ps') + 
                                       te(Longitude, Latitude)")

# Formula with the autoregressive term s(count)
# NOTE: Assumes your data has a column named 'count' representing the previous time step's count.
formula_autoregressive <- as.formula("count_t1 ~ s(tmax, bs = 'tp', k=6) + 
                                                  s(prcp, bs = 'ts') + 
                                                  s(wspd, bs = 'ps') + 
                                                  s(week_num, bs = 'ps') + 
                                                  te(Longitude, Latitude) +
                                                  s(count)")


# --- 2. Fit both models on the training data ---

cat("Fitting the base model (without autoregressive term)...\n")
model_base <- gam(formula_base,
                  family = gaussian(),
                  data = train_data,
                  method = "REML",
                  select = TRUE)

cat("Fitting the autoregressive model...\n")
model_autoregressive <- gam(formula_autoregressive,
                            family = gaussian(),
                            data = train_data,
                            method = "REML",
                            select = TRUE)

cat("Models fitted successfully.\n\n")


# --- 3. Generate predictions on the test set ---

# Ensure your test data is clean and available (e.g., test_data_clean)
predictions_base <- predict(model_base, newdata = test_data_clean, type = "response")
predictions_autoregressive <- predict(model_autoregressive, newdata = test_data_clean, type = "response")

# Get the actual values from the test set
actuals <- test_data_clean$count_t1


# --- 4. Calculate performance metrics (RMSE and R-squared) ---

# Function to calculate RMSE
calculate_rmse <- function(predictions, actuals) {
  sqrt(mean((predictions - actuals)^2))
}

# Function to calculate R-squared
calculate_r2 <- function(predictions, actuals) {
  ssr <- sum((predictions - actuals)^2)
  sst <- sum((actuals - mean(actuals))^2)
  1 - (ssr / sst)
}

# Calculate metrics for the base model
rmse_base <- calculate_rmse(predictions_base, actuals)
r2_base <- calculate_r2(predictions_base, actuals)

# Calculate metrics for the autoregressive model
rmse_autoregressive <- calculate_rmse(predictions_autoregressive, actuals)
r2_autoregressive <- calculate_r2(predictions_autoregressive, actuals)


# --- 5. Display the results in a clean table ---

results_df <- data.frame(
  Model = c("Base Model", "Autoregressive Model"),
  RMSE = c(rmse_base, rmse_autoregressive),
  R_squared = c(r2_base, r2_autoregressive)
)

cat("--- Performance on Test Set ---\n")
print(results_df, row.names = FALSE)


# --- 6. Generate LaTeX table for the report ---

cat("\n--- LaTeX Code for Report ---\n")

# Create an xtable object
latex_table <- xtable(results_df,
                      caption = "Comparison of Model Performance on the Test Set",
                      label = "tab:model_performance_comparison",
                      digits = 4)

# Print the LaTeX code to the console
print(latex_table, 
      include.rownames = FALSE, 
      comment = FALSE)

