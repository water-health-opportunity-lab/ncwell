# ----------------------------------------
# R Script to extract various predictors from raw data in hazard module
# Indicators:
#   - Satellite-based inundation map
#   - impact from superfund sites
#   - impact from CAFOs
#   - impact from TRI water releases
#   - impact from TRI total releases
#   - impact from OWTS
#   - impact from agricultural runoff
#   - floodplain map
#
# Author: Xindi Hu & Kyndra Shea
# Last edited: 2025-05-06
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
# nc_grid_sample <- nc_grid %>%
# slice_sample(n = 10000)

handlers("txtprogressbar")
with_progress({
  nc_grid <- compute_impact_score(points_sf = sems_sf,
                                 grid_sf = nc_grid,
                                 threshold_m = 5000,
                                 weight_var = NULL,
                                 output_var = "sems_impact")
})

# full run takes 316 seconds
# check the result
summary(nc_grid$sems_impact)

# WRITE OUT RESULTS 
st_write(nc_grid, "data/output/nc_grid_hazard.gpkg", layer = "hazard", delete_layer = TRUE)

### EXTRACT IMPACTS FROM CAFO LOCATIONS

## 1. nc_grid is already in memory

## 2. read in point sources
cafo_df <- read_excel("data/input/CAFOs/List_OfPermitted_Animal_Facilities20241104.xlsx", sheet = 1) %>%
  janitor::clean_names()  # convert column names to snake casa using the janitor package

# Remove rows with missing lat/long
cafo_df_clean <- cafo_df %>%
  filter(!is.na(location_lat_num) & !is.na(location_long_num))

# Convert to spatial points (assuming WGS84 / EPSG:4326)
cafo_sf <- st_as_sf(cafo_df_clean, coords = c("location_long_num", "location_lat_num"), crs = 4326)

## 3. Compute CAFO impact score
# take a sample to test the code
# nc_grid_sample <- nc_grid %>%
# slice_sample(n = 5000)

handlers("txtprogressbar")
with_progress({
  nc_grid <- compute_impact_score(points_sf = cafo_sf,
                                  grid_sf = nc_grid,
                                  threshold_m = 5000,
                                  weight_var = NULL,
                                  output_var = "cafo_impact")
})

# full run takes 1504 seconds
# check results
summary(nc_grid$cafo_impact)

## 4. WRITE OUT RESULTS
st_write(nc_grid, "data/output/nc_grid_hazard.gpkg", layer = "hazard", delete_layer = TRUE)

### EXTRACT IMPACTS FROM TRI WATER RELEASES

## 1. nc_grid is already in memory

## 2. read in point sources
tri_df <- readxl::read_excel("data/input/TRI impact/TRI Toxic Tracker_2019 - 2023.xlsx", sheet = 1) %>%
  janitor::clean_names()  # convert column names to snake casa using the janitor package

# Filter out rows missing coordinates
tri_df_clean <- tri_df %>%
  filter(!is.na(longitude), !is.na(latitude))

# Convert to spatial points (assuming WGS84 / EPSG:4326)
tri_sf <- st_as_sf(tri_df_clean, coords = c("longitude", "latitude"), crs = 4326)

## 3. Compute impact score with water release as weight (lbs)
# take a sample to test the code
# nc_grid_sample <- nc_grid %>%
# slice_sample(n = 5000)

handlers("txtprogressbar")
with_progress({
  nc_grid <- compute_impact_score(
    points_sf = tri_sf,
    grid_sf = nc_grid,
    threshold_m = 5000,
    weight_var = "water_releases_lb",
    output_var = "TRI.water.releases"
  )
})

# full run takes 2177 seconds
# check result
summary(nc_grid$TRI.water.releases)

## 4. WRITE OUT RESULTS
st_write(nc_grid, "data/output/nc_grid_hazard.gpkg", layer = "hazard", delete_layer = TRUE)

### EXTRACT IMPACTS FROM TRI TOTAL RELEASES

## 1. nc_grid is already in memory

## 2. tri_df is already in memory

# tri_sf is already in memory

## 3. Compute impact score with total releases as weight (lbs)
# take a sample to test the code
# nc_grid_sample <- nc_grid %>%
# slice_sample(n = 5000)

handlers("txtprogressbar")
with_progress({
  nc_grid <- compute_impact_score(
    points_sf = tri_sf,
    grid_sf = nc_grid,
    threshold_m = 5000,
    weight_var = "total_on_site_releases_lb",
    output_var = "TRI.total.releases"
  )
})

# full run takes 1797 seconds
# check result
summary(nc_grid$TRI.total.releases)

## 4. WRITE OUT RESULTS
st_write(nc_grid, "data/output/nc_grid_hazard.gpkg", layer = "hazard", delete_layer = TRUE)

### EXTRACT IMPACTS FROM ONSITE WASTEWATER TREATMENT SYSTEMS

## 1. nc_grid is already in memory

## 2. read in point sources
owts_df <- readxl::read_excel("data/input/Onsite Wastewater Treatment Systems/NPDES_Wastewater_Discharge_Permits_-4938817765183004059.xlsx") %>%
  janitor::clean_names()

# Convert to spatial points (assuming WGS84 / EPSG:4326)
owts_sf <- st_as_sf(owts_df, coords = c("long", "lat"), crs = 4326)

## 3. Compute OWTS impact score
handlers("txtprogressbar")
with_progress({
  nc_grid <- compute_impact_score(
    points_sf = owts_sf,
    grid_sf = nc_grid,
    threshold_m = 5000,
    weight_var = NULL,
    output_var = "owts"
  )
})

# full run takes 1650 seconds
# check results
summary(nc_grid$owts)

## 4. WRITE OUT RESULTS
st_write(nc_grid, "data/output/nc_grid_hazard.gpkg", layer = "hazard", delete_layer = TRUE)

### EXTRACT IMPACTS FROM AGRICULTURAL RUNOFF

## 1. nc_grid is already in memory

## 2. read in raster
# read in cropland data layer
cdl_raster <- rast("data/input/Agricultural Runoff/CDL_2024_37.tif")

## 3. align CRS
# reproject raster to match the coordinate reference system of nc_grid
cdl_raster_reprojected <- project(cdl_raster, vect(nc_grid), method = "near")

## 4. create nitrogen application table
# crop codes for corn = 1, potatoes - 42 (based on USDA CDL codes)
# assign 164 lbs/acres to corn and potatoes; 56 other crops; 0 to nonag areas
# define code groups using USDA. CDL codes
high_nitrogen_crops <- c(1, 42)  # corn, potatoes → 164 lbs/acre average
zero_nitrogen_codes <- c(
  0, 254, 255,        # No data, cloud cover
  82:87,              # Developed areas
  83, 92,             # Water, aquaculture
  122, 131,           # Non-ag/undefined
  111,                # Barren/rock/sand
  190, 195,           # Wetlands
  210                 # Perennial ice/snow
)

# get unique crop codes
unique_codes <- unique(values(cdl_raster))
unique_codes <- unique_codes[!is.na(unique_codes)]

# apply the nitrogen application rates
nitrogen_lookup <- setNames(
  sapply(unique_codes, function(code) {
    if (code %in% high_nitrogen_crops) {
      164
    } else if (code %in% zero_nitrogen_codes) {
      0
    } else {
      56
    }
  }),
  unique_codes
)

# apply lookup to raster to create a nitrogen raster
# convert raster values to nitrogen rates
nitrogen_raster <- classify(cdl_raster_reprojected, rcl = cbind(unique_codes, nitrogen_lookup))

## 4. extract the mean nitrogen per grid cell
nc_grid$ag_runoff <- exact_extract(nitrogen_raster, nc_grid, 'mean')
# check the result
summary(nc_grid$ag_runoff)

