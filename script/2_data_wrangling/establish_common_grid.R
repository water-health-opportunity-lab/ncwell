# ----------------------------------------
# R Script to estabish the base grid for NC well contamination risk map
# Author: Xindi Hu
# Last edited: 2025-04-18
# ----------------------------------------

# Install and load necessary packages (if you haven't already)
if (!require("sf")) renv::install("sf")
if (!require("terra")) renv::install("terra")
if (!require("dplyr")) renv::install("dplyr")
if (!require("maps")) renv::install("maps")
library(sf)
library(terra)
library(dplyr)
library(maps)

Sys.setenv(PROJ_LIB = "/usr/local/share/proj") 

# 1. Get North Carolina's Boundary (using built-in data for demonstration)
# Get the North Carolina boundary and reproject to UTM zone 17N
nc_boundary <- st_as_sf(map("state", "north carolina", plot = FALSE, fill = TRUE)) %>%
  st_transform(crs = 32617) # UTM zone 17N (suitable for NC)

# 2. Define Grid Cell Size (1km x 1km)
cell_size <- 1000 # in meters

# 3. Create Grid
# Get the bounding box of North Carolina
nc_bbox <- st_bbox(nc_boundary)

# Create a sequence of x and y coordinates for the grid
x_coords <- seq(from = nc_bbox$xmin, to = nc_bbox$xmax, by = cell_size)
y_coords <- seq(from = nc_bbox$ymin, to = nc_bbox$ymax, by = cell_size)

# Create a grid of points
grid_points <- expand.grid(x = x_coords, y = y_coords)

# Convert the points to an sf object
grid_sf <- st_as_sf(grid_points, coords = c("x", "y"), crs = st_crs(nc_boundary))

# Create a grid of polygons (squares)
grid_polygons <- st_make_grid(grid_sf, cellsize = cell_size)

# Convert the grid to an sf object
grid_sf <- st_sf(geometry = grid_polygons)

# 4. Clip Grid to North Carolina's Boundary
clipped_grid <- st_intersection(grid_sf, nc_boundary)

# Optional: Add a unique ID to each grid cell
clipped_grid$grid_id <- 1:nrow(clipped_grid)

# Optional: Visualize the grid (if you want to see it)
plot(st_geometry(nc_boundary), main = "North Carolina Grid")
plot(st_geometry(clipped_grid), add = TRUE, col = "lightblue")

st_write(clipped_grid, "data/output/nc_grid.shp")
