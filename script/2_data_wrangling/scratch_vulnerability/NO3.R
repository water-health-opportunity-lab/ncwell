# read in helper function
source(here::here("script/2_data_wrangling/helper_functions.R"))

# EXTRACT DRAINAGE
## 1. read in grid shapefile
# read in nc_grid, the common base grid for North Carolina
nc_grid <- st_read("data/output/nc_grid/nc_grid.shp")

no3_dom <- rast ("/Users/ellenwei/ncwell/data/input/USGS NO3/no3_doms.asc")
no3_pub <- rast ("/Users/ellenwei/ncwell/data/input/USGS NO3/no3_pubs.asc")

## 3. align CRS
# reproject raster to match the coordinate reference system of nc_grid
no3_dom_reprojected <- project(no3_dom, vect(nc_grid), method = "bilinear")

## 3. align CRS
# reproject raster to match the coordinate reference system of nc_grid
no3_pub_reprojected <- project(no3_pub, vect(nc_grid), method = "bilinear")

## 4. extract raster values using mean per polygon
# exactextractr handles partial overlaps and weighting
# variable name "tmean" needs to match what is in the tracker on OneDrive
nc_grid$tmean <- exact_extract(tmean_reprojected, nc_grid, 'mean')

## 4. extract raster values using mean per polygon
# exactextractr handles partial overlaps and weighting
# variable name "ppt" needs to match what is in the tracker on OneDrive
nc_grid$ppt <- exact_extract(ppt_reprojected, nc_grid, 'mean')

# check the result
summary(nc_grid$tmean)
plot(nc_grid['tmean'], border = NA)

# check the result
summary(nc_grid$ppt)
plot(nc_grid['ppt'], border = NA)