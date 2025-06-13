# ----------------------------------------
# R Script to extract various predictors from raw data in hazard module
# Indicators:
#   - satellite-based inundation map
#   - impact from superfund sites
#   - impact from CAFOs
#   - impact from TRI water releases
#   - impact from TRI total releases
#   - impact from OWTS
#   - impact from agricultural runoff
#   - floodplain map
#
# Author: Xindi Hu & Kyndra Shea
# Last edited: 2025-06-08
# ----------------------------------------
# read in helper function
source(here::here("script/2_data_wrangling/helper_functions.R"))

### EXTRACT FLOOD INUNDATION

## 1. read in shapefile
# read in nc_grid, the common base grid for North Carolina
nc_grid <- st_read("data/output/nc_grid/nc_grid.shp")

## 2. read in rasters
# read in 40 flood inundation rasters
tif_files <- list.files("data/input/Satellite-based inundation map/", 
                        pattern = "^Flood_NC_.*\\.tif$", full.names = TRUE)

## 3. align CRS
# reproject rasters to match the coordinate reference system of nc_grid
reprojected_rasters <- lapply(tif_files, function(file) {
  r <- rast(file)
  project(r, vect(nc_grid), method = "bilinear")
})

## 4. stack the rasters and calculate max across layers
# combine into a SpatRaster stack
r_stack <- rast(reprojected_rasters)

# use terra::app to compute max across all raster layers (ignoring NA)
r_max <- app(r_stack, fun = max, na.rm = TRUE)

## 5. extract max value per grid cell
nc_grid$inundation_max <- exact_extract(r_max, nc_grid, 'mean')

# check the result
summary(nc_grid$inundation_max)

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
    output_var = "tri_water_releases"
  )
})

# full run takes 2177 seconds
# check result
summary(nc_grid$tri_water_releases)

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
    output_var = "tri_total_releases"
  )
})

# full run takes 1797 seconds
# check result
summary(nc_grid$tri_total_releases)

### EXTRACT IMPACTS FROM ONSITE WASTEWATER TREATMENT SYSTEMS

## 1. nc_grid is already in memory

## 2. read in point sources
owts_df <- readxl::read_excel("data/input/Onsite Wastewater Treatment Systems/NPDES_Wastewater_Discharge_Permits_-4938817765183004059.xlsx") %>%
  janitor::clean_names()  # convert column names to snake casa using the janitor package

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

### 100-YR FLOOD MAP
## 1. nc_grid is already in memory, get grid cell centroids
nc_centroids_geom <- st_centroid(st_geometry(nc_grid))  # this avoids the warning
nc_centroids <- st_sf(nc_grid, geometry = nc_centroids_geom)

## 2. read in FEMA 100-year flood zone shapefile
flood_shp <- st_read("data/input/FEMA Floodplain/North_Carolina_Flood_Hazard_Area_Effective.shp") %>%
  select(ZONE_LID)

# filter to 1% annual chance flood zones (100-year flood)
flood_100yr <- flood_shp %>%
  filter(ZONE_LID %in% c("1001", "1005", "1003", "1009", "1000"))

## 3. align CRS
flood_100yr_reprojected <- st_transform(flood_100yr, st_crs(nc_grid))

## 4. Spatial join: assign polygon attribute(s) to each centroid
# This gives each centroid the value of the polygon it falls in
nc_centroids_joined <- st_join(nc_centroids, flood_100yr_reprojected, left = TRUE) %>%
  st_drop_geometry() %>%
  mutate(fema_floodplain = !is.na(ZONE_LID)) %>%
  select(grid_id, fema_floodplain)

# 5. Merge the joined values back into the original grid
nc_grid <- nc_grid %>%
  left_join(nc_centroids_joined, by = "grid_id")

# check the result
table(nc_grid$fema_floodplain, useNA = "ifany")

# WRITE OUT RESULTS 
st_write(nc_grid, "data/output/nc_grid_hazard.gpkg", layer = "hazard", delete_layer = TRUE)