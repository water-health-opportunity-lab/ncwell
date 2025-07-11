source(here::here("script/2_data_wrangling/helper_functions.R"))

nc_grid <- st_read("data/output/nc_grid/nc_grid.shp")

# EXTRACT HUC12
## 1. nc_grid is already in memory, get grid cell centroids
nc_centroids_geom <- st_centroid(st_geometry(nc_grid))  # this avoids the warning
nc_centroids <- st_sf(nc_grid, geometry = nc_centroids_geom)

## 2. read in HUC 12 shapefile
huc12 <- st_read("/Users/ellenwei/ncwell/data/input/Landscape/12_Digit_HUC_Subwatersheds/12_Digit_HUC_Subwatersheds.shp")

huc12 <- huc12 %>%  
  select(HUC_12)

## 3. align CRS
huc12_reprojected <- st_transform(huc12, st_crs(nc_grid))

nc_centroids_joined <- st_join(nc_centroids, huc12_reprojected) %>%
  st_drop_geometry() %>%
  dplyr::select(grid_id, huc12 = HUC_12)

nc_grid <- nc_grid %>%
  left_join(nc_centroids_joined, by = "grid_id")

table(nc_grid$huc12, useNA = "ifany")
plot(nc_grid['huc12'], border = NA)

summary(nc_grid$huc12)
plot(nc_grid['huc12'], border = NA)

