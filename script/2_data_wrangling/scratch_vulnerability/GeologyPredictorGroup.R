# GEOLOGY PREDICTOR GROUP
#   - drainage (drainage)
#   - surficial geology (surfgeo) 
#   - bedrock geology (KB)
#   - lithology (lith)

# read in helper function
source(here::here("script/2_data_wrangling/helper_functions.R"))

# EXTRACT DRAINAGE
## 1. read in grid shapefile
# read in nc_grid, the common base grid for North Carolina
nc_grid <- st_read("data/output/nc_grid/nc_grid.shp")

## 2. read in drainage raster
drainage_raster <- rast("/Users/ellenwei/ncwell/data/input/Geology/Drainage/usgs_tiledrainage/SubsurfaceDrainExtentUS_90s_agg.tif")

## 3. align CRS
# reproject raster to match the coordinate reference system of nc_grid
drainage_raster_reprojected <- project(drainage_raster, vect(nc_grid), method = "bilinear")

## 4. extract raster values using mean per polygon
# exactextractr handles partial overlaps and weighting
# variable name "drainage" needs to match what is in the tracker on OneDrive
nc_grid$drainage <- exact_extract(drainage_raster_reprojected, nc_grid, 'mean')

# check the result
summary(nc_grid$drainage)
plot(nc_grid['drainage'], border = NA)

#SURFACE GEOLOGY

# EXTRACT SURFACE GEOLOGY
## 1. nc_grid is already in memory, get grid cell centroids
nc_centroids_geom <- st_centroid(st_geometry(nc_grid))  # this avoids the warning
nc_centroids <- st_sf(nc_grid, geometry = nc_centroids_geom)

## 2. read in surface geology shapefile
surfgeo_shp <- st_read("/Users/ellenwei/ncwell/data/input/Geology/Surficial Geology/Surficial_materials.shp") %>%
  select("UNIT_NAME")

## 3. align CRS
surfgeo_shp_reprojected <- st_transform(surfgeo_shp, st_crs(nc_grid))

## 4. Spatial join: assign polygon attribute(s) to each centroid
# This gives each centroid the value of the polygon it falls in
nc_centroids_joined <- st_join(nc_centroids, surfgeo_shp_reprojected) %>%
  st_drop_geometry() %>%
  dplyr::select(grid_id, surfgeo = UNIT_NAME)

# 5. Merge the joined values back into the original grid
nc_grid <- nc_grid %>%
  left_join(nc_centroids_joined, by = "grid_id")

# check the result
table(nc_grid$surfgeo, useNA = "ifany")
plot(nc_grid['surfgeo'], border = NA)

#BEDROCK GEOLOGY
## 2. read in bedrock geology shapefile
KB_shp <- st_read("/Users/ellenwei/ncwell/data/input/Geology/Bedrock/KBGE/kbge.shp")%>%
  select("UNIT")

## 3. align CRS
# reproject raster to match the coordinate reference system of nc_grid
KB_shp_reprojected <- st_transform(KB_shp, st_crs(nc_grid))

## 4. Spatial join: assign polygon attribute(s) to each centroid
# This gives each centroid the value of the polygon it falls in
nc_centroids_joined <- st_join(nc_centroids, KB_shp_reprojected) %>%
  st_drop_geometry() %>%
  dplyr::select(grid_id, KB = UNIT)

# 5. Merge the joined values back into the original grid
nc_grid <- nc_grid %>%
  left_join(nc_centroids_joined, by = "grid_id")

# check the result
table(nc_grid$KB, useNA = "ifany")
plot(nc_grid['KB'], border = NA)

# LITHOLOGY
## 2. read in lithology  shapefile
lith_shp <- st_read("/Users/ellenwei/ncwell/data/input/Geology/Lithology/geol_poly.shp") %>%
  select("LITH62")

## 3. align CRS
# reproject raster to match the coordinate reference system of nc_grid
lith_shp_reprojected <- st_transform(lith_shp, st_crs(nc_grid))

## 4. Spatial join: assign polygon attribute(s) to each centroid
# This gives each centroid the value of the polygon it falls in
nc_centroids_joined <- st_join(nc_centroids, lith_shp_reprojected) %>%
  st_drop_geometry() %>%
  dplyr::select(grid_id, lith = LITH62)

# 5. Merge the joined values back into the original grid
nc_grid <- nc_grid %>%
  left_join(nc_centroids_joined, by = "grid_id")

# check the result
table(nc_grid$lith, useNA = "ifany")
plot(nc_grid['lith'], border = NA)

#WRITE OUT RESULTS 
st_write(nc_grid, "data/output/nc_grid_vulnerability.gpkg", layer = "vulnerability", delete_layer = TRUE)
