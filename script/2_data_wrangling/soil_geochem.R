# ----------------------------------------
# R Script to extract A Horizon soil geochemistry to NC grid
# Author: Kyndra Shea
# Last edited: 2025-05-28
# ----------------------------------------

library(tidyverse)
library(sf)
library(readxl)
library(janitor)
library(raster)
library(exactextractr)
library(parallel)

## 1. read in NC grid shapefile
nc_grid <- st_read("data/output/nc_grid/nc_grid.shp")

## 2. define list of geochemical variables
chemNameList <- c(
  'A_Sb', 'A_As', 'A_Be', 'A_Cd', 'A_Calcite', 'A_Ca', 'A_Cr', 'A_Hornbl', 'A_C_Inorg',
  'A_Fe', 'A_Kaolinit', 'A_Pb', 'A_Li', 'A_Mn', 'A_Mg', 'A_Hg', 'A_Mo', 'A_C_Org',
  'A_P', 'A_K', 'A_Rb', 'A_Na', 'A_Sr', 'A_Ti', 'A_Tot_10A', 'A_Tot_14A', 'A_C_Tot',
  'A_Tot_Clay', 'A_Tot_Plag', 'A_Tot_K_fs', 'A_U', 'A_V'
)

## 3. read and prepare A Horizon data
a_horizon <- read_excel("data/input/Soil Geochemistry/Appendix_3a_Ahorizon_18Sept2013-2.xlsx",
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

## 5. define function to interpolate and extract values
extract_soil_geochem <- function(chemName) {
  message(paste("Processing:", chemName))
  geochemOut <- as(geochem_sf_transformed, "Spatial")
  r <- raster(extent(geochemOut), res = 1000, crs = proj4string(geochemOut))  # 1km grid
  r <- rasterizefromXYZ(geochemOut, r, field = chemName, fun = mean)
  result <- exact_extract(r, nc_grid, 'mean')
  return(result)
}
#test here to make sure that the function works, before scaling up to all the items
A_Sb_result <- extract_soil_geochem(chemName = 'A_Sb')

## 6. apply function across all chemical variables
results_list <- mclapply(chemNameList, extract_soil_geochem, mc.cores = 6) # running here
names(results_list) <- chemNameList

## 7. bind results to nc_grid
nc_grid <- bind_cols(nc_grid, as.data.frame(results_list))

## 8. export final geochem grid
st_write(nc_grid, "data/output/nc_grid_geochem.gpkg", layer = "geochem", delete_layer = TRUE)

## 9. confirmation
print("Soil geochemistry raster extraction completed.")

# as long as it has a new name, push to GIT and add it to the hazard branch and then Dr. Hu will take it and look into what Jillei did and if it can work here
