# read in helper function
source(here::here("script/2_data_wrangling/helper_functions.R"))

# EXTRACT DRAINAGE
## 1. read in grid shapefile
# read in nc_grid, the common base grid for North Carolina
nc_grid <- st_read("data/output/nc_grid/nc_grid.shp")


# EXTRACT SURFACE GEOLOGY
## 1. nc_grid is already in memory, get grid cell centroids
nc_centroids_geom <- st_centroid(st_geometry(nc_grid))  # this avoids the warning
nc_centroids <- st_sf(nc_grid, geometry = nc_centroids_geom)

## 2. read in surface geology shapefile
landuse_shp <- st_read("/Users/ellenwei/ncwell/data/input/Landscape/NC_strata_17/NC_strata_17.shp") %>%
  select(Strata)

## 3. align CRS
landuse_shp_reprojected <- st_transform(landuse_shp, st_crs(nc_grid))

## 4. Spatial join: assign polygon attribute(s) to each centroid
# This gives each centroid the value of the polygon it falls in
nc_centroids_joined <- st_join(nc_centroids, landuse_shp_reprojected) %>%
  st_drop_geometry() %>%
  dplyr::select(grid_id, landuse_shp = Strata)

# 5. Merge the joined values back into the original grid
nc_grid <- nc_grid %>%
  left_join(nc_centroids_joined, by = "grid_id")

# check the result
table(nc_grid$landuse, useNA = "ifany")
plot(nc_grid['landuse_shp'], border = NA)

# Read in Land Cover
landcover <- read.csv("/Users/ellenwei/ncwell/data/input/Landscape/LandCover_CONUS.csv")

