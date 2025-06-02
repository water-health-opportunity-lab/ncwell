# ----------------------------------------
# R Script to extract A Horizon soil geochemistry to NC grid
# Author: Kyndra Shea & Cindy Hu
# Last edited: 2025-06-02
# ----------------------------------------

library(tidyverse)
library(sf)
library(readxl)
library(janitor)
library(raster)
library(terra)
library(gstat)
library(exactextractr)
library(parallel)

## 1. read in NC grid shapefile
nc_grid <- st_read("data/output/nc_grid/nc_grid.shp")
# create centroidAdd commentMore actions
nc_grid_centroids <- st_centroid(nc_grid)
# keep only the geometry column — remove all extra attributes
nc_grid_centroids <- st_sf(geometry = st_geometry(nc_grid_centroids))

## 2. define list of geochemical variables
chemNameList <- c(
  'A_Sb', 'A_As', 'A_Be', 'A_Cd', 'A_Calcite', 'A_Ca', 'A_Cr', 'A_Hornbl', 'A_C_Inorg',
  'A_Fe', 'A_Kaolinit', 'A_Pb', 'A_Li', 'A_Mn', 'A_Mg', 'A_Hg', 'A_Mo', 'A_C_Org',
  'A_P', 'A_K', 'A_Rb', 'A_Na', 'A_Sr', 'A_Ti', 'A_Tot_10A', 'A_Tot_14A', 'A_C_Tot',
  'A_Tot_Clay', 'A_Tot_Plag', 'A_Tot_K_fs', 'A_U', 'A_V'
)

## 3. read and prepare A Horizon data
a_horizon <- read_excel("data/input/Soil Geochemistry/Appendix_3a_Ahorizon_18Sept2013-2.xls",
                        sheet = "A_Horizon",
                        skip = 12) %>%
  filter(StateID == "NC") %>%
  mutate(across(all_of(chemNameList), as.numeric),
         longitude = as.numeric(Longitude),
         latitude = as.numeric(Latitude)) %>%
  filter(!is.na(Longitude) & !is.na(Latitude))

## 4. convert to sf and transform to match nc_grid
geochem_sf <- st_as_sf(a_horizon, coords = c("longitude", "latitude"), crs = 4326)
geochem_sf_transformed <- st_transform(geochem_sf, st_crs(nc_grid))


# create regular grid over bounding box
bbox <- st_bbox(geochem_sf_transformed)
cellsize <- 5000  # 5 km
buffer_dist <- 60000
expanded_bbox <- bbox
expanded_bbox[c("xmin", "ymin")] <- expanded_bbox[c("xmin", "ymin")] - buffer_dist
expanded_bbox[c("xmax", "ymax")] <- expanded_bbox[c("xmax", "ymax")] + buffer_dist
grid <- st_make_grid(
  st_as_sfc(expanded_bbox), 
  cellsize = cellsize, 
  what = "centers", 
  square = TRUE
)
grid_sf <- st_sf(geometry = grid)  # Ensure it's an sf object

# convert both points and grid to Spatial (gstat prefers sp format)
pts_sp <- as(geochem_sf_transformed, "Spatial")
grid_sp <- as(grid_sf, "Spatial")

## 5. define function to interpolate and extract values
extract_soil_geochem <- function(chemName) {
  message(paste("Processing:", chemName))
  pts_sp_clean <- pts_sp[!is.na(pts_sp[[chemName]]), ]
  
  # fit inverse-distance weighting model and predict
  idw_model <- gstat(formula = as.formula(paste(chemName, "~ 1")), data = pts_sp_clean)
  
  predicted <- predict(idw_model, newdata = grid_sp)
  
  r <- rasterize(vect(st_as_sf(predicted)), #vector
                 rast(vect(st_as_sf(predicted)), res = cellsize), #raster
                 field = "var1.pred")
  result <- exact_extract(r, nc_grid, "mean")
  return(result)
}

# test here to make sure that the function works, before scaling up to all the items
A_Sb_result <- extract_soil_geochem(chemName = 'A_Sb')
summary(A_Sb_result)

## 6. apply function across all chemical variables
results_list <- lapply(chemNameList, extract_soil_geochem)
names(results_list) <- chemNameList

## 7. bind results to nc_grid
nc_grid <- bind_cols(nc_grid, as.data.frame(results_list))

## 8. export final geochem grid
st_write(nc_grid, "data/output/nc_grid_geochem.gpkg", layer = "geochem", delete_layer = TRUE)
