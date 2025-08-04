# ----------------------------------------
# R Script to extract various predictors from raw data in vulnerability module
# Indicators:
#   - drainage
#   - surficial geology
# Author: Xindi Hu
# Last edited: 2025-04-22
# ----------------------------------------
# read in helper function
source(here::here("script/2_data_wrangling/helper_functions.R"))

# EXTRACT DRAINAGE
## 1. read in grid shapefile
# read in nc_grid, the common base grid for North Carolina
nc_grid <- st_read("data/output/nc_grid/nc_grid.shp")

## 2. read in well use csv
well_use <- read.csv("/Users/ellenwei/ncwell/data/input/Well Use/Well_Estimates_2020_Blocks.csv", stringsAsFactors = FALSE)

welluse_nc <- well_use %>%
  filter(State == "NC")

# 4. Ensure GEOID_Block is properly formatted (15 digits)
welluse_nc$GEOID_Block <- sprintf("%015s", welluse_nc$GEOID_Block)
