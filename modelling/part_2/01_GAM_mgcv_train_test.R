# GAM

# SCRIPT CONFIGURATION


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

# Bis Hier******-----------------------

## choose best interaction term-----
# Your current model using te()
mod_te <- gam(count ~ te(Longitude, Latitude, k = optimal_k) +
                s(elevation, k = optimal_k) +
                s(tavg, k = optimal_k) +
                s(tmax, k = optimal_k) +
                s(prcp, k = optimal_k) +
                s(wdir, k = optimal_k) +
                s(wspd, k = optimal_k) +
                s(pres, k = optimal_k), 
              family = poisson, data = df, method = "REML")

# The alternative model using s()
mod_s <- gam(count ~ s(Longitude, Latitude, k = optimal_k) +
               s(elevation, k = optimal_k) +
               s(tavg, k = optimal_k) +
               s(tmax, k = optimal_k) +
               s(prcp, k = optimal_k) +
               s(wdir, k = optimal_k) +
               s(wspd, k = optimal_k) +
               s(pres, k = optimal_k), 
             family = poisson, data = df, method = "REML")

# Compare the two models
AIC(mod_te, mod_s)

# Loop Basis Function-----------

# Define your response and covariates
response <- "count"
covariates <- c("elevation", "tavg", "tmax", "prcp", "wdir", "wspd", "pres")

# Candidate smoothing bases
bases <- c("tp", "cr", "cs", "ps", "ts")

# Store best bases
best_bases <- list()

# Loop through covariates individually, choosing best smoothing basis per covariate
for(var in covariates){
  aic_vals <- c()
  
  for(b in bases){
    # Construct the model formula dynamically
    formula_str <- paste0(response, "~ te(Longitude, Latitude, k=7) + ")
    formula_str <- paste0(formula_str, paste0("s(", var, ", bs='", b, "') + "))
    
    # Add other variables with default "tp"
    other_vars <- covariates[covariates != var]
    formula_str <- paste0(formula_str, paste0("s(", other_vars, ", bs='tp')", collapse=" + "))
    
    # Fit the GAM with REML or ML for AIC comparison (REML is default but use ML for strict AIC optimization)
    mod <- gam(as.formula(formula_str), family=poisson, data=df, method="ML")
    
    # Save AIC
    aic_vals[b] <- AIC(mod)
  }
  
  # Select basis with lowest AIC
  best_basis <- names(which.min(aic_vals))
  best_bases[[var]] <- best_basis
}

# Display the selected best basis per variable
print(best_bases)

# Fit final GAM using optimal smoothing basis per covariate
final_formula_str <- paste0(response, "~ te(Longitude, Latitude, k=7) + ")
final_formula_str <- paste0(final_formula_str, paste0("s(", covariates, ", bs='", best_bases, "')", collapse=" + "))

final_gam <- gam(as.formula(final_formula_str), family=poisson, data=df, method="REML")
AIC(final_gam, final_mod)

# Check final model
summary(final_gam)

# plot model with best basis fn
plot(final_gam,pages=2,scheme=2, scale = 0) ## alternative visualization
plot(final_gam,pages=2,scheme=1, scale = 0) ## alternative visualization
# plot model with best k (df)
plot(final_mod,pages=2,scheme=2, scale = 0) ## alternative visualization