# ----------------------------------------
# R Script to extract various predictors from raw data in capacity module
# Indicators:
#   - census tract level indicators
#     - pct_poverty
#     - pct_women_gave_birth
#     - pct_insured
#     - pct_us_born
#     - pct_no_move
#     - pct_gov_workers
#
# Author: Xindi Hu
# Last edited: 2025-05-03
# ----------------------------------------

# read in helper function
source(here::here("script/2_data_wrangling/helper_functions.R"))

# load package that only this script needs
library(tidycensus)
options(tigris_use_cache = TRUE) # cache shapefiles for use in future session

# EXTRACT CENSUS TRACT ACS VARIABLES
## 1. read in grid shapefile
# download the nc_grid folder from OneDrive, https://gwu0-my.sharepoint.com/:f:/r/personal/g40775607_gwu_edu/Documents/Projects/REACH%20pilot/2-aims/aim1/2_raw_data/nc_grid?csf=1&web=1&e=vDLixm
# create a data folder in the project directory, under ncwell
# create an input folder and an output folder under data
# save the nc_grid zip file in the output folder, unzip
# read in nc_grid, the common base grid for North Carolina
nc_grid <- st_read("data/output/nc_grid/nc_grid.shp")
nc_centroids_geom <- st_centroid(st_geometry(nc_grid))  # this avoids the warning
nc_centroids <- st_sf(nc_grid, geometry = nc_centroids_geom)

## 2. read in acs tabular data
# download the ACS folder from OneDrive, https://gwu0-my.sharepoint.com/:f:/r/personal/g40775607_gwu_edu/Documents/Projects/REACH%20pilot/2-aims/aim1/2_raw_data/Capacity/ACS?csf=1&web=1&e=fNpDp7
# save the ACS zip file in the input folder, unzip
df_acs_tract <- read_csv("data/input/ACS/acs_tract.csv") %>%
  # convert column names to snake case using the janitor package
  janitor::clean_names() %>%
  mutate(geoid = as.character(geoid))

# add the geometry column to the data frame
nc_tract <- get_acs(
  geography = "tract",
  state = "NC",
  year = 2023,
  survey = "acs5",
  variables = "B05001_001",
  geometry = TRUE) %>%
  dplyr::select(geoid = GEOID, geometry)

# join tabular ACS data to geometry
sf_acs_tract <- nc_tract %>%
  left_join(df_acs_tract, by = "geoid")

## 3. align CRS
sf_acs_tract_reprojected <- st_transform(sf_acs_tract, st_crs(nc_grid))

## 4. Spatial join: assign polygon attribute(s) to each centroid
# This gives each centroid the value of the polygon it falls in
nc_centroids_joined_tract <- st_join(nc_centroids, sf_acs_tract_reprojected) %>%
  st_drop_geometry() %>%
  dplyr::select(grid_id, tract_name = name, starts_with('pct_'))

# 5. Merge the joined values back into the original grid
nc_grid <- nc_grid %>%
  left_join(nc_centroids_joined_tract, by = "grid_id")

# EXTRACT BLOCK GROUP ACS VARIABLES
## 2. read in acs tabular data
df_acs_block_group <- read_csv("data/input/ACS/acs_block_group.csv") %>%
  janitor::clean_names() %>%
  mutate(geoid = as.character(geoid))

# add the geometry column to the data frame
nc_block_group <- get_acs(
  geography = "block group",
  state = "NC",
  year = 2023,
  survey = "acs5",
  variables = "B02001_001",
  geometry = TRUE) %>%
  dplyr::select(geoid = GEOID, geometry)

# join tabular ACS data to geometry
sf_acs_block_group <- nc_block_group %>%
  left_join(df_acs_block_group, by = "geoid")

## 3. align CRS
sf_acs_block_group_reprojected <- st_transform(sf_acs_block_group, st_crs(nc_grid))

## 4. Spatial join: assign polygon attribute(s) to each centroid
# This gives each centroid the value of the polygon it falls in
nc_centroids_joined_block_group <- st_join(nc_centroids, sf_acs_block_group_reprojected) %>%
  st_drop_geometry() %>%
  dplyr::select(grid_id, block_group_name = name, starts_with(c('pct_','pop_','housing_','households_')))

# 5. Merge the joined values back into the original grid
nc_grid <- nc_grid %>%
  left_join(nc_centroids_joined_block_group, by = "grid_id") %>%
  relocate("block_group_name", .after = "grid_id") %>%
  select(-"tract_name")

# check the result
# summary(nc_grid$pop_density)
# plot(nc_grid['pop_density'], border = NA)

#WRITE OUT RESULTS 
st_write(nc_grid, "data/output/nc_grid_capacity.gpkg", layer = "capacity", delete_layer = TRUE)
# upload to the .gpkg file to OneDrive 
# https://gwu0-my.sharepoint.com/:f:/r/personal/g40775607_gwu_edu/Documents/Projects/REACH%20pilot/2-aims/aim1/3_processed_data/Capacity?csf=1&web=1&e=GqcDg6


