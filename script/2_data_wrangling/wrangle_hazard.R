# ----------------------------------------
# R Script to extract various predictors from raw data in hazard module
# Indicators:
#   - Satellite-based inundation map
#   - impact from superfund sites
#
# Author: Xindi Hu
# Last edited: 2025-04-18
# ----------------------------------------
# read in helper function
source(here::here("script/2_data_wrangling/helper_functions.R"))

### EXTRACT FLOOD INUNDATION

## 1. read in shapefile
# read in nc_grid, the common base grid for North Carolina
nc_grid <- st_read("data/output/nc_grid/nc_grid.shp")

## 2. read in raster
# read in flood inundation raster
flood_inundation_raster <- rast("data/input/Satellite-based inundation map/Flood_NC_2024092721.tif")

## 3. align CRS
# reproject raster to match the coordinate reference system of nc_grid
flood_inundation_raster_reprojected <- project(flood_inundation_raster, vect(nc_grid), method = "bilinear")

## 4. extract raster values using mean per polygon
# exactextractr handles partial overlaps and weighting
# variable name "inundation" needs to match what is in the tracker on OneDrive
nc_grid$inundation <- exact_extract(flood_inundation_raster_reprojected, nc_grid, 'mean')
# check the result
summary(nc_grid$inundation)

### EXTRACT IMPACTS FROM SUPERFUND SITES

## 1. nc_grid is already in memory

## 2. read in point sources, if lat/long are not available, need to geocode first
sems_df <- read_excel("data/input/Superfund/sems.xlsx", sheet = 1) %>%
  # convert column names to snake casa using the janitor package
  janitor::clean_names()
# If stored in degrees (e.g., WGS84), use EPSG:4326
sems_sf <- st_as_sf(sems_df, coords = c("longitude", "latitude"), crs = 4326)

## 3. call the impact function
# take a sample to test the code
nc_grid_sample <- nc_grid %>%
  slice_sample(n = 10000)

handlers("txtprogressbar")
with_progress({
  nc_grid_sample <- compute_impact_score(points_sf = sems_sf,
                                 grid_sf = nc_grid_sample,
                                 threshold_m = 5000,
                                 weight_var = NULL,
                                 output_var = "sems_impact")
})

# full run takes 316 seconds
# check the result
summary(nc_grid$sems_impact)

# WRITE OUT RESULTS 
st_write(nc_grid, "data/output/nc_grid_hazard.gpkg", layer = "hazard", delete_layer = TRUE)

# test

