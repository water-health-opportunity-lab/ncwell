# GROUNDWATER PREDICTOR GROUP
#   - Depth to Water (dtw) 
#   - Transmissivity (trans)
#   - Unsat Zone Travel Time (unsatTT)
#   - Unsat Zone Water Content (unsatWC)
#   - Aquifer Rock Type (aq_rocktype)

#   - Nitrate, domestic well depth (no3_dom)
#   - Nitrate, public well depth (no3_pub)

library(tmap)

# read in helper function
source(here::here("script/2_data_wrangling/helper_functions.R"))

# EXTRACT DRAINAGE
## 1. read in grid shapefile
# read in nc_grid, the common base grid for North Carolina
nc_grid <- st_read("data/output/nc_grid/nc_grid.shp")

### EXTRACT DTW
## 2. read in raster
# read in dtw raster
dtw_raster <- rast("/Users/ellenwei/ncwell/data/input/Groundwater/DTW/conus_MF6_SS_Unconfined_250_dtw.tif")

## 3. align CRS
# reproject raster to match the coordinate reference system of nc_grid
dtw_raster_reprojected <- project(dtw_raster, vect(nc_grid), method = "bilinear")

## 4. extract raster values using mean per polygon
# exactextractr handles partial overlaps and weighting
# variable name "dtw" needs to match what is in the tracker on OneDrive
nc_grid$dtw <- exact_extract(dtw_raster_reprojected, nc_grid, 'mean')

# check the result
summary(nc_grid$dtw)
plot(nc_grid['dtw'], border = NA)

## TRANS RASTER
## 2. read in raster
# read in dtw raster
trans_raster <- rast("/Users/ellenwei/ncwell/data/input/Groundwater/Transmissivity/conus_MF6_SS_Unconfined_250_trans.tif")

## 3. align CRS
# reproject raster to match the coordinate reference system of nc_grid
trans_raster_reprojected <- project(trans_raster, vect(nc_grid), method = "bilinear")

## 4. extract raster values using mean per polygon
# exactextractr handles partial overlaps and weighting
# variable name "trans" needs to match what is in the tracker on OneDrive
nc_grid$trans <- exact_extract(trans_raster_reprojected, nc_grid, 'mean')

# check the result
summary(nc_grid$trans)
plot(nc_grid['trans'], border = NA)

## UnsatTT RASTER
## 2. read in raster
# read in unsattt raster
unsatTT_raster <- rast("/Users/ellenwei/ncwell/data/input/Groundwater/TT/conus_MF6_SS_Unconfined_250_tt_total.tif")

## 3. align CRS
# reproject raster to match the coordinate reference system of nc_grid
unsatTT_raster_reprojected <- project(unsatTT_raster, vect(nc_grid), method = "bilinear")

## 4. extract raster values using mean per polygon
# exactextractr handles partial overlaps and weighting
# variable name "unsattt" needs to match what is in the tracker on OneDrive
nc_grid$unsatTT <- exact_extract(unsatTT_raster_reprojected, nc_grid, 'mean')

# check the result
summary(nc_grid$unsatTT)
plot(nc_grid['unsatTT'], border = NA)

## UnsatWC RASTER
## 2. read in raster
# read in unsatwc raster
unsatWC_raster <- rast("/Users/ellenwei/ncwell/data/input/Groundwater/WC/conus_MF6_SS_Unconfined_250_wc_avg.tif")

## 3. align CRS
# reproject raster to match the coordinate reference system of nc_grid
unsatWC_raster_reprojected <- project(unsatWC_raster, vect(nc_grid), method = "bilinear")

## 4. extract raster values using mean per polygon
# exactextractr handles partial overlaps and weighting
# variable name "unsatwc" needs to match what is in the tracker on OneDrive
nc_grid$unsatWC <- exact_extract(unsatWC_raster_reprojected, nc_grid, 'mean')

# check the result
summary(nc_grid$unsatWC)
plot(nc_grid['unsatWC'], border = NA)

### Read in Aquifer
aq_rocktype <- st_read("/Users/ellenwei/ncwell/data/input/Groundwater/Aquifer/us_aquifers.shp") %>%
  select("ROCK_TYPE")

## 3. align CRS
aq_rocktype_reprojected <- st_transform(aq_rocktype, st_crs(nc_grid))

## 4. Spatial join: assign polygon attribute(s) to each centroid
# This gives each centroid the value of the polygon it falls in
nc_centroids_geom <- st_centroid(st_geometry(nc_grid))  # this avoids the warning
nc_centroids <- st_sf(nc_grid, geometry = nc_centroids_geom)

nc_centroids_joined <- st_join(nc_centroids, aq_rocktype_reprojected) %>%
  st_drop_geometry() %>%
  dplyr::select(grid_id, aq_rocktype = ROCK_TYPE)

# 5. Merge the joined values back into the original grid
nc_grid <- nc_grid %>%
  left_join(nc_centroids_joined, by = "grid_id")

# check the result
table(nc_grid$aq_rocktype, useNA = "ifany")
plot(nc_grid['aq_rocktype'], border = NA)


### READ IN NO3 
no3_dom <- rast ("/Users/ellenwei/ncwell/data/input/USGS NO3/no3_doms.asc") %>%
no3_pub <- rast ("/Users/ellenwei/ncwell/data/input/USGS NO3/no3_pubs.asc")

## 3. align CRS
# reproject raster to match the coordinate reference system of nc_grid
no3_pub_reprojected <- project(no3_pub, vect(nc_grid), method = "bilinear")

## 4. extract raster values using mean per polygon
# exactextractr handles partial overlaps and weighting
# variable name "tmean" needs to match what is in the tracker on OneDrive
nc_grid$no3_dom <- exact_extract(no3_dom_reprojected, nc_grid, 'mean')
nc_grid$no3_pub <- exact_extract(no3_pub_reprojected, nc_grid, 'mean')

# check the result
summary(nc_grid$no3_dom)
plot(nc_grid['no3_dom'], border = NA)

summary(nc_grid$no3_pub)
plot(nc_grid['no3_pub'], border = NA)

# check the result
summary(nc_grid$ppt)
plot(nc_grid['ppt'], border = NA)




