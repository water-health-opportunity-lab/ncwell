# ----------------------------------------
# R Script to fill in missing data with K Nearest Neighbors imputation
# Author: Jennifer Zhang
# Last edited: 2025-06-25
# ----------------------------------------

library(sf)
<<<<<<< HEAD
=======
library(dplyr)
>>>>>>> df36910cc55719bbc7f40df8345f96dd7f1234fe
library(VIM)
library(ggplot2)

# Read in data from module
data <- st_read('data/output/nc_grid_capacity.gpkg')

# Put centroids in a second geometry column
data$centroids <- st_centroid(data$geom)
coords <- st_coordinates(data$centroids)

# Add coordinates
data$x_coord <- coords[, 1]
data$y_coord <- coords[, 2]

# List of variable columns to impute
cols <- names(data)[4:28] # adjust for other modules

# Impute data with K Nearest Neighbors algorithm
imputed_data <- kNN(data,
                    variable=cols,
<<<<<<< HEAD
                    k=5, # can be adjusted
=======
                    k=10, # can be adjusted
>>>>>>> df36910cc55719bbc7f40df8345f96dd7f1234fe
                    dist_var=c("x_coord","y_coord"),
                    imp_var=FALSE)

data[cols] <- imputed_data[cols]

# Check results
# plot(data['pct_poverty'], border=NA)
# colSums(is.na(data))

st_write(data, "data/output/nc_grid_capacity_imputed.gpkg", layer="capacity", delete_layer=TRUE)