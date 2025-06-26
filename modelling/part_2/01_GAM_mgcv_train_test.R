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

# Candidate k-values for hyperparameter tuning
CANDIDATE_K <- c(5, 7, 10, 15, 20)

# Number of folds for cross-validation
CV_FOLDS <- 5

# Set a seed for all random processes for reproducibility
set.seed(42)



# 3. DATA LOADING AND PRE-PROCESSING-----------

cat("--- Loading and pre-processing data ---\n")

# Load the raw data
full_df <- read.csv(FILE_PATH)

# Create 'departamento' column and convert to a spatial 'sf' object
# The coordinate system (CRS) 4326 is standard for Latitude/Longitude data.
full_sf <- full_df %>%
  mutate(
    departamento = str_split_i(idx_city, "_", 2) # More direct way to get the 2nd element
  ) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

cat("Data loaded and converted to spatial object.\n")



# 4. TRAIN-TEST SPLIT BY DEPARTMENT---------------

# This creates the final hold-out test set, which will not be touched
# during model tuning.

cat("\n--- Creating train-test split based on departments ---\n")

# Get unique departments and sample them for the test set
all_deps <- unique(full_sf$departamento)
test_departamentos <- sample(all_deps, size = floor(TEST_PROPORTION * length(all_deps)))

# Create the training and testing sets
train_data <- full_sf %>% filter(!departamento %in% test_departamentos)
test_data  <- full_sf %>% filter(departamento %in% test_departamentos)

# Report summary of the split
cat("Split complete:\n")
cat("  - Training data:", nrow(train_data), "rows from", n_distinct(train_data$departamento), "departments.\n")
cat("  - Testing data: ", nrow(test_data), "rows from", n_distinct(test_data$departamento), "departments.\n")


## clean NaNs---------------
library(tidyr)
predictor_cols <- c('count', 'Latitude', 'Longitude', "tavg", "tmax", "prcp", "wdir", "wspd", "pres", "elevation")
test_data_clean <- test_data %>%
  drop_na(all_of(predictor_cols))

# 5. Best K loop--------------------------------
# HYPERPARAMETER TUNING (FINDING BEST K) VIA AIC - PARALLEL `foreach` VERSION
# This section uses ONLY the 'train_data' to find the best `k` for the model
# by fitting a model for each candidate `k` and choosing the one with the lowest AIC.
# The process is parallelized to speed up computation.

## 1. Load libraries for parallel processing ------
# install.packages(c("foreach", "doParallel")) # Run once if not installed
library(foreach)
library(doParallel)
library(mgcv)
library(tibble)

## 2. Set up the parallel environment ------
# Use all cores minus one to keep the computer responsive
n_cores <- parallel::detectCores() - 1
my_cluster <- makeCluster(n_cores)
registerDoParallel(my_cluster) # Register the cluster for foreach

cat(paste("\n--- Starting PARALLEL tuning to find best k using", n_cores, "cores (foreach) ---\n"))

## 3. Define candidates and run the parallel tuning loop -------
# Candidate values for k
candidate_k <- c(5, 7, 10, 15, 20)

# The `foreach` loop replaces a standard `for` loop.
# - `%dopar%` tells it to run iterations in parallel.
# - `.combine = 'rbind'` gathers the results from each core into a single data frame.
# - `.packages` ensures each parallel worker has the necessary libraries loaded.
aic_results <- foreach(
  k_val = candidate_k, 
  .combine = 'rbind', 
  .packages = c('mgcv', 'tibble')
) %dopar% {
  
  # This block of code is executed on a separate core for each `k_val`.
  
  # Define the formula with the current k value
  current_formula <- as.formula(
    paste0("count ~ te(Longitude, Latitude, k = ", k_val, ") +",
           "s(elevation, k = ", k_val, ") + s(tavg, k = ", k_val, ") + s(tmax, k = ", k_val, ") +",
           "s(prcp, k = ", k_val, ") + s(wdir, k = ", k_val, ") + s(wspd, k = ", k_val, ") +",
           "s(pres, k = ", k_val, ")")
  )
  
  # Fit the GAM model on the ENTIRE training dataset
  model_fit <- gam(current_formula, 
                   family = poisson, 
                   data = train_data, 
                   method = "REML")
  
  # Extract the AIC 
  current_aic <- AIC(model_fit)
  
  # Return a one-row tibble for this k_val. `foreach` will combine these.
  return(tibble(k = k_val, AIC = current_aic))
  
} # End of foreach loop

## 4. Shut down parallel workers and show results --------
# It's very important to stop the cluster to release the computer's resources.
stopCluster(my_cluster)
cat("\nParallel processing complete.\n")

cat("\n--- Tuning Results ---\n")
print(aic_results)

# Select the optimal k based on the lowest AIC score
#optimal_k_aic <- aic_results$k[which.min(aic_results$AIC)] # result is 20
optimal_k_aic <- 20 # uncomment and coment the loop to avoid time
cat(paste("\nOptimal k selected by AIC:", optimal_k_aic, "\n"))

## 6. FINAL MODEL TRAINING--------------------------------
# Now we train the definitive model on the *entire* training dataset
# using the best hyperparameter (k) we just found via AIC.

cat("\n--- Training final model on all training data ---\n")

# Redefine formula with the now-known optimal k
final_formula <- as.formula(
  paste0("count ~ te(Longitude, Latitude, k = ", optimal_k_aic, ") +",
         "s(elevation, k = ", optimal_k_aic, ") + s(tavg, k = ", optimal_k_aic, ") + s(tmax, k = ", optimal_k_aic, ") +",
         "s(prcp, k = ", optimal_k_aic, ") + s(wdir, k = ", optimal_k_aic, ") + s(wspd, k = ", optimal_k_aic, ") +",
         "s(pres, k = ", optimal_k_aic, ")")
)

# Train the final model
final_model <- gam(final_formula, 
                   family = poisson, 
                   data = train_data, 
                   method = "REML")

cat("Final model trained successfully using k =", optimal_k_aic, "\n")

# Print the model summary to inspect its components
cat("\n--- Final Model Summary ---\n")
print(summary(final_model))


## 7. VISUALIZE MODEL EFFECTS-------------------------
# Plot the smooth terms of the final model to understand the relationships
# between each predictor and the outcome.

cat("\n--- Plotting final model smooths... ---\n")

# plot.gam() visualizes each smooth component.
# - `pages=2` spreads the plots over multiple pages to make them readable.
# - `scheme=2` uses a color scheme with shaded confidence intervals.
# - `scale=0` puts all plots on the same y-axis scale for easier comparison.
plot(final_model, pages = 2, scheme = 2, scale = 0)


## 8. FINAL MODEL EVALUATION ON TEST DATA-------------------------
# This is the final validation step. We use the model trained on `train_data`
# to make predictions on the completely unseen `test_data`.

cat("\n--- Evaluating performance on hold-out test set ---\n")

# --- 1. Clean the test data to avoid errors with NAs ---
# It's crucial to remove any rows with missing data from the test set 
# before making predictions.
predictor_cols <- c('count', 'Latitude', 'Longitude', "tavg", "tmax", "prcp", "wdir", "wspd", "pres", "elevation")
test_data_clean <- test_data %>%
  tidyr::drop_na(all_of(predictor_cols))

cat(paste("Original test rows:", nrow(test_data), "| Cleaned test rows for evaluation:", nrow(test_data_clean), "\n"))


# --- 2. Make predictions on the cleaned test data ---
# `type = "response"` ensures the predictions are on the original count scale.
test_predictions <- predict(final_model, newdata = test_data_clean, type = "response")
actual_values <- test_data_clean$count


# --- 3. Calculate and report performance metrics ---
# Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((test_predictions - actual_values)^2))

# R-squared (R²), calculated as the square of the correlation between actuals and predictions.
# This is a common way to assess R² for non-linear models.
r_squared <- cor(test_predictions, actual_values)^2

cat("\n--- Test Set Performance Metrics ---\n")
cat(paste("  - RMSE:", round(rmse, 3), "\n"))
cat(paste("  - R-squared (R²):", round(r_squared, 4), "\n"))



# BIS HIER********************--------------

# Best Basis Fn --------------------------------------
# HYPERPARAMETER TUNING (FINDING BEST BASIS FUNCTION `bs`) VIA AIC
# This section uses ONLY the 'train_data'

cat("\n--- Starting hyperparameter tuning to find the best basis function (`bs`) for each covariate ---\n")

# --- 1. Define the search space ---

# Covariates to optimize
covariates_to_tune <- c("elevation", "tavg", "tmax", "prcp", "wdir", "wspd", "pres")

# Candidate basis functions to test
candidate_bases <- c("tp", "cr", "cs", "ps", "ts") 

# Fixed k for this tuning approach. A moderate value is chosen.
K_FIXED <- 20

# --- 2. Run the tuning loop ---

# A list to store the best basis function found for each covariate
best_bases <- list()

# Loop through each covariate
for (current_var in covariates_to_tune) {
  
  aic_scores <- c() # To store AIC values for the current variable
  cat(paste0("--- Tuning basis for: '", current_var, "' ---\n"))
  
  # Loop through each candidate basis for the current covariate
  for (current_basis in candidate_bases) {
    
    # Dynamically build the formula string
    # Start with the spatial term
    formula_str <- paste0("count ~ te(Longitude, Latitude, k = ", K_FIXED, ")")
    
    # Add the smooth term for the variable we are currently tuning
    formula_str <- paste0(formula_str, " + s(", current_var, ", bs='", current_basis, "', k=", K_FIXED, ")")
    
    # Add all other covariates with a default basis ('tp')
    other_vars <- covariates_to_tune[covariates_to_tune != current_var]
    other_smooths <- paste0("s(", other_vars, ", bs='tp', k=", K_FIXED, ")", collapse = " + ")
    
    final_formula_str <- paste(formula_str, other_smooths, sep = " + ")
    
    # Fit the GAM using ML for strict AIC comparison
    # Use the entire training data for AIC calculation
    model_fit <- gam(as.formula(final_formula_str), 
                     family = poisson, 
                     data = train_data, 
                     method = "ML") # Use ML for comparing models with different smooths via AIC
    
    # Store the AIC score
    aic_scores[current_basis] <- AIC(model_fit)
  }
  
  # Find and store the basis with the lowest AIC for the current variable
  best_basis_for_var <- names(which.min(aic_scores))
  best_bases[[current_var]] <- best_basis_for_var
  
  cat(paste0("  -> Best basis for '", current_var, "': '", best_basis_for_var, "' (AIC: ", round(min(aic_scores), 1), ")\n\n"))
}

cat("--- Basis Function Tuning Complete ---\n")
print(unlist(best_bases))


## FINAL MODEL TRAINING --------------------------

cat("\n--- Training final model using the best basis functions found ---\n")

# Build the final formula string using the list of best bases
final_formula_basis <- "count ~ te(Longitude, Latitude, k = K_FIXED)"
for (var in names(best_bases)) {
  final_formula_basis <- paste0(final_formula_basis, " + s(", var, ", bs='", best_bases[[var]], "', k=", K_FIXED, ")")
}

# Train the final model on the ENTIRE training dataset using REML for better estimation
final_gam_model_basis <- gam(as.formula(final_formula_basis),
                             family = poisson,
                             data = train_data,
                             method = "REML")

cat("Final model (basis function strategy) trained successfully.\n")


## FINAL MODEL EVALUATION----------------

# --- 1. Inspect the model summary ---
cat("\n--- Final Model Summary (Basis Function Strategy) ---\n")
print(summary(final_gam_model_basis))

# --- 2. Evaluate performance on the held-out test set ---
cat("\n--- Evaluating performance on hold-out test set ---\n")

test_predictions_basis <- predict(final_gam_model_basis, newdata = test_data, type = "response")
test_rmse_basis <- sqrt(mean((test_predictions_basis - test_data$count)^2))
test_cor_basis <- cor(test_predictions_basis, test_data$count)

cat(paste("  - Final Test Set RMSE:", round(test_rmse_basis, 3), "\n"))
cat(paste("  - Final Test Set Correlation:", round(test_cor_basis, 3), "\n"))

# --- 3. Plot the smooth effects of the final model ---
cat("\nPlotting final model smooths...\n")
plot(final_gam_model_basis, pages = 2, scheme = 2, scale = 0)

# Model Comparison --------------





