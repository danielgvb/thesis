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

# 5. Model --------------------------------------------------



# formula to get time as important
#final_formula <- as.formula("count_t1 ~ s(week_num, bs = 'ps') + 
#                                        te(Longitude, Latitude)")

final_formula <- as.formula("count_t1 ~ s(tmax, bs = 'tp', k=6) + 
                                       s(prcp, bs = 'ts') + 
                                       s(wspd, bs = 'ps') + 
                                       s(week_num, bs = 'ps') + 
                                       te(Longitude, Latitude)")

# --- 1. DEFINE AND FIT THE NEW INTERACTION MODEL ---

final_formula <- as.formula("count_t1 ~ s(tmax, bs = 'tp', k=6) + 
                                       s(prcp, bs = 'ts') + 
                                       s(wspd, bs = 'ps') + 
                                       te(Longitude, Latitude, week_num)")

# Fit the final, optimized model. 
# Note: This interaction model is more complex and will take longer to fit.
cat("--- Fitting the spatio-temporal interaction model... ---\n")
final_model <- gam(final_formula,
                   family = gaussian(),
                   data = train_data,
                   method = "REML",
                   select = TRUE)
cat("--- Model fitting complete. ---\n")

plot(final_model, scheme = 2, pages = 2)

# --- 2. LOAD REQUIRED LIBRARIES ---
library(ggplot2)
library(sf)
library(rnaturalearth)
library(dplyr)
library(tidyr)


# --- 3. PREPARE THE MAP AND PREDICTION GRID ---

country_map <- ne_countries(scale = "large", country = "Colombia", returnclass = "sf")

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

clipped_grid <- prediction_grid %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(country_map)) %>%
  st_intersection(country_map)

clipped_coords <- st_coordinates(clipped_grid)
clipped_grid$Longitude <- clipped_coords[, "X"]
clipped_grid$Latitude <- clipped_coords[, "Y"]


# --- 4. SETUP THE ANIMATION ---

if (!dir.exists("animation_frames")) {
  dir.create("animation_frames")
}

week_sequence <- round(seq(
  from = min(train_data$week_num), 
  to = max(train_data$week_num), 
  length.out = 50 # Increase for a smoother video
))


# --- 5. BUILD THE PREDICTION GRID FOR THE INTERACTION TERM ---
cat("--- Preparing data grid for prediction... ---\n")
# We only need space and time for the interaction term
full_prediction_grid <- tidyr::crossing(
  clipped_grid %>% select(Longitude, Latitude, geometry),
  week_num = week_sequence
)

# Add the mean values of other covariates. This is standard practice
# when predicting the effect of a single term.
full_prediction_grid$tmax <- mean(train_data$tmax, na.rm = TRUE)
full_prediction_grid$prcp <- mean(train_data$prcp, na.rm = TRUE)
full_prediction_grid$wspd <- mean(train_data$wspd, na.rm = TRUE)


# --- 6. PREDICT THE SPATIO-TEMPORAL EFFECT ---
cat("--- Predicting the isolated spatio-temporal effect for each frame... ---\n")
# Use type = "terms" to get the effect of our interaction term ONLY
all_effects <- predict(
  final_model,
  newdata = full_prediction_grid,
  type = "terms"
)

# Add the effect to our grid. The column name must match the term exactly.
full_prediction_grid$spatial_time_effect <- all_effects[, "te(Longitude,Latitude,week_num)"]

# Calculate the global range for a consistent color scale
effect_range <- range(full_prediction_grid$spatial_time_effect, na.rm = TRUE)
cat("Global effect range for color scale:", round(effect_range, 2), "\n")

# Add a readable date column for plot titles
full_prediction_grid <- full_prediction_grid %>% 
  mutate(week_date = as.Date(week_num, origin = "1970-01-01"))


# --- 7. GENERATE AND SAVE PLOTS IN A LOOP ---
cat("--- Starting to generate and save animation frames... ---\n")
for (i in 1:length(week_sequence)) {
  current_week_num <- week_sequence[i]
  plot_data <- full_prediction_grid %>% 
    filter(week_num == current_week_num)
  current_date <- plot_data$week_date[1]
  
  p <- ggplot() +
    geom_raster(data = plot_data, aes(x = Longitude, y = Latitude, fill = spatial_time_effect)) +
    geom_sf(data = country_map, fill = NA, color = "black", linewidth = 0.5) +
    
    # Use the requested RdBu palette, which is excellent for term plots
    scale_fill_distiller(
      palette = "RdBu",
      name = "Spatio-temporal\nEffect",
      limits = effect_range
    ) +
    
    coord_sf(crs = st_crs(country_map)) +
    
    # Update titles to be specific about what is being shown
    labs(
      title = "Spatio-temporal Interaction Effect on Dengue",
      subtitle = paste("Date:", format(current_date, "%Y-%m-%d")),
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14)
    )
  
  file_name <- sprintf("animation_frames/frame_%03d.png", i)
  ggsave(file_name, plot = p, width = 8, height = 7, dpi = 150)
  cat("Saved:", file_name, "\n")
}

cat("--- Frame generation complete! --- \n")
cat("You can now use the Python script or FFmpeg to create your dynamic video. 🎬\n")




# --- 7. GENERATE AND SAVE PLOTS IN A LOOP ---
cat("--- Starting to generate and save animation frames... ---\n")

# Get the map's bounding box to help place the text
map_bbox <- st_bbox(country_map)

for (i in 1:length(week_sequence)) {
  current_week_num <- week_sequence[i]
  plot_data <- full_prediction_grid %>% 
    filter(week_num == current_week_num)
  current_date <- plot_data$week_date[1]
  
  # Format the date string for the plot
  date_label <- format(current_date, "%d %b %Y") # e.g., "20 Aug 2025"
  
  p <- ggplot() +
    geom_raster(data = plot_data, aes(x = Longitude, y = Latitude, fill = spatial_time_effect)) +
    geom_sf(data = country_map, fill = NA, color = "black", linewidth = 0.5) +
    
    scale_fill_distiller(
      palette = "RdBu",
      name = "Spatio-temporal\nEffect",
      limits = effect_range
    ) +
    
    # --- NEW CODE TO ADD THE DATE ---
    # This adds a label with a white, semi-transparent background to the bottom-left corner.
    annotate(
      "label",
      x = map_bbox["xmin"] + 0.5, # X-coordinate (slightly in from the left edge)
      y = map_bbox["ymin"] + 0.5, # Y-coordinate (slightly up from the bottom edge)
      label = date_label,        # The text to display
      fontface = "bold",         # Make the font bold
      color = "black",           # Text color
      fill = alpha("white", 0.6),# Background color (60% transparent white)
      hjust = 0                  # Horizontal justification (0 = left)
    ) +
    # ---------------------------------
  
  coord_sf(crs = st_crs(country_map)) +
    
    labs(
      title = "Spatio-temporal Interaction Effect on Dengue",
      subtitle = "Spatial risk pattern shown for the date below", # Subtitle is now more general
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 14)
    )
  
  file_name <- sprintf("animation_frames/frame_%03d.png", i)
  ggsave(file_name, plot = p, width = 8, height = 7, dpi = 150)
  cat("Saved:", file_name, "\n")
}

cat("--- Frame generation complete! --- \n")
