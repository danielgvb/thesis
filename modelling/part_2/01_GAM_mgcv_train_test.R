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



# DATA LOADING AND PRE-PROCESSING-----------

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



# TRAIN-TEST SPLIT BY DEPARTMENT---------------

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



# HYPERPARAMETER TUNING (FINDING BEST K) VIA SPATIAL CV------------------

# This section uses ONLY the 'train_data'

cat("\n--- Setting up for hyperparameter tuning using spatial CV ---\n")

# Create spatial cross-validation folds from the training data
cv_folds <- spatial_block_cv(train_data, v = CV_FOLDS)
cat(paste("Created", CV_FOLDS, "spatial cross-validation folds.\n"))

# Define the GAM formula once to avoid repeating it
gam_formula <- as.formula(
  count ~ te(Longitude, Latitude, k = k_val) +
    s(elevation, k = k_val) + s(tavg, k = k_val) + s(tmax, k = k_val) +
    s(prcp, k = k_val) + s(wdir, k = k_val) + s(wspd, k = k_val) +
    s(pres, k = k_val)
)

# Data frame to store tuning results
cv_results <- tibble(k = integer(), mean_rmse = double(), sd_rmse = double())

cat("\n--- Starting cross-validation to find the best k ---\n")

# Loop through each candidate k
for (k_val in CANDIDATE_K) {
  
  fold_rmse_scores <- c()
  cat(paste0("--- Evaluating k = ", k_val, " ---\n"))
  
  # Loop through each CV fold
  for (i in 1:CV_FOLDS) {
    analysis_set <- training(cv_folds$splits[[i]])
    assessment_set <- testing(cv_folds$splits[[i]])
    
    # Fit the model on the analysis set
    model_fit <- gam(gam_formula, family = poisson, data = analysis_set, method = "REML")
    
    # Predict on the assessment set and calculate RMSE
    predictions <- predict(model_fit, newdata = assessment_set, type = "response")
    fold_rmse <- sqrt(mean((predictions - assessment_set$count)^2))
    fold_rmse_scores <- c(fold_rmse_scores, fold_rmse)
  }
  
  # Store the average performance for this k
  cv_results <- cv_results %>%
    add_row(k = k_val, mean_rmse = mean(fold_rmse_scores), sd_rmse = sd(fold_rmse_scores))
}

cat("\n--- Cross-Validation Complete ---\n")
print(cv_results)

# Select the optimal k based on the lowest mean RMSE
optimal_k_cv <- cv_results$k[which.min(cv_results$mean_rmse)]
cat(paste("\nOptimal k selected by Cross-Validation:", optimal_k_cv, "\n"))



# FINAL MODEL TRAINING-------------

# Now we train the definitive model on the *entire* training dataset
# using the best hyperparameter (k) we just found.

cat("\n--- Training final model on all training data ---\n")

# Redefine formula with the now-known optimal k
final_formula <- as.formula(
  count ~ te(Longitude, Latitude, k = optimal_k_cv) +
    s(elevation, k = optimal_k_cv) + s(tavg, k = optimal_k_cv) + s(tmax, k = optimal_k_cv) +
    s(prcp, k = optimal_k_cv) + s(wdir, k = optimal_k_cv) + s(wspd, k = optimal_k_cv) +
    s(pres, k = optimal_k_cv)
)

# Train the final model
final_gam_model <- gam(final_formula, family = poisson, data = train_data, method = "REML")

cat("Final model trained successfully.\n")



# FINAL MODEL EVALUATION-------------


# --- 1. Inspect the model summary ---
cat("\n--- Final Model Summary ---\n")
print(summary(final_gam_model))

# --- 2. Evaluate performance on the held-out test set ---
cat("\n--- Evaluating performance on hold-out test set ---\n")

test_predictions <- predict(final_gam_model, newdata = test_data, type = "response")
test_rmse <- sqrt(mean((test_predictions - test_data$count)^2))
test_cor <- cor(test_predictions, test_data$count)

cat(paste("  - Final Test Set RMSE:", round(test_rmse, 3), "\n"))
cat(paste("  - Final Test Set Correlation:", round(test_cor, 3), "\n"))

# --- 3. Plot the smooth effects of the final model ---
cat("\nPlotting final model smooths...\n")
plot(final_gam_model, pages = 2, scheme = 2, scale = 0)

# Best Basis Fn -------------------------
# HYPERPARAMETER TUNING (FINDING BEST BASIS FUNCTION `bs`) VIA AIC
# This section uses ONLY the 'train_data'

cat("\n--- Starting hyperparameter tuning to find the best basis function (`bs`) for each covariate ---\n")

# --- 1. Define the search space ---

# Covariates to optimize
covariates_to_tune <- c("elevation", "tavg", "tmax", "prcp", "wdir", "wspd", "pres")

# Candidate basis functions to test
candidate_bases <- c("tp", "cr", "cs", "ps", "ts") 

# Fixed k for this tuning approach. A moderate value is chosen.
K_FIXED <- 7

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

