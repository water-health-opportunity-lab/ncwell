# ----------------------------------------
# R Script to fill in missing data with K Nearest Neighbors imputation for Capacity Module
# Author: Jennifer Zhang
# Last edited: 2025-06-25
# ----------------------------------------

library(sf)
library(dplyr)
library(FNN)
library(ggplot2)

# Read in data from module
data <- st_read('data/output/nc_grid_capacity_transformed_no_impute.gpkg')

# Put centroids in a second geometry column
data$centroids <- st_centroid(data$geom)
coords <- st_coordinates(data$centroids)

# List of variable columns to impute
cols <- names(data)[4:27]

# Impute data with K Nearest Neighbors algorithm
# ----------------------------------------
data <- data %>% mutate(row_id = row_number())  # preserve row order

k_choice = 5

for (var in cols){
  na_rows <- which(is.na(data[[var]]))
  complete_rows <- which(!is.na(data[[var]]))
  
  nn <- get.knnx(data = coords[complete_rows, ], query = coords[na_rows, ], k = k_choice)
  neighbors <- nn$nn.index
  
  for (j in seq_along(na_rows)){
    i <- na_rows[j]
    neighbor_idx <- complete_rows[neighbors[j, ]]
    selected_vals <- data[[var]][neighbor_idx]
    
    data[[var]][i] <- mean(selected_vals, na.rm = TRUE)
  }
}

data <- data %>% select(-row_id)

# Check results
# plot(data['pct_poverty'], border=NA)
# colSums(is.na(data))

st_write(data, "data/output/nc_grid_capacity_imputed.gpkg", layer="capacity", delete_layer=TRUE)