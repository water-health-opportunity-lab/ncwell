# read in helper function
source(here::here("script/2_data_wrangling/helper_functions.R"))

### EXTRACT DTW
## 1. read in shapefile
# read in nc_grid, the common base grid for North Carolina
nc_grid <- st_read("data/output/nc_grid/nc_grid.shp")

## 2. read in rasters
# read in EF raster
ef_raster <- rast("/Users/ellenwei/ncwell/data/input/Reitz/Effective Recharge/RC_eff_0013.tif")

# read in et raster
et_raster <- rast("/Users/ellenwei/ncwell/data/input/Reitz/Evapotranspiration/ET_0013.tif")

# read in nr raster
nr_raster <- rast("/Users/ellenwei/ncwell/data/input/Reitz/Net Recharge/NetRecharge.tif")

# read in qf raster
qf_raster <- rast("/Users/ellenwei/ncwell/data/input/Reitz/Quick Flow/RO_0013.tif")

# read in sg raster
sg_raster <- rast("/Users/ellenwei/ncwell/data/input/Reitz/Surface Geology/SurfGeo.tif")

## 3. align CRS
# reproject raster to match the coordinate reference system of nc_grid
ef_raster_reprojected <- project(ef_raster, vect(nc_grid), method = "bilinear")

## 4. extract raster values using mean per polygon
# exactextractr handles partial overlaps and weighting
# variable name "ef" needs to match what is in the tracker on OneDrive
nc_grid$ef <- exact_extract(ef_raster_reprojected, nc_grid, 'mean')

# check the result
summary(nc_grid$ef)
plot(nc_grid['ef'], border = NA)

## 3. align CRS
# reproject raster to match the coordinate reference system of nc_grid
et_raster_reprojected <- project(et_raster, vect(nc_grid), method = "bilinear")

## 4. extract raster values using mean per polygon
# exactextractr handles partial overlaps and weighting
# variable name "et" needs to match what is in the tracker on OneDrive
nc_grid$et <- exact_extract(et_raster_reprojected, nc_grid, 'mean')

# check the result
summary(nc_grid$et)
plot(nc_grid['et'], border = NA)


## 3. align CRS
# reproject raster to match the coordinate reference system of nc_grid
nr_raster_reprojected <- project(nr_raster, vect(nc_grid), method = "bilinear")

## 4. extract raster values using mean per polygon
# exactextractr handles partial overlaps and weighting
# variable name "nc" needs to match what is in the tracker on OneDrive
nc_grid$nr <- exact_extract(nr_raster_reprojected, nc_grid, 'mean')

# check the result
summary(nc_grid$nr)
plot(nc_grid['nr'], border = NA)

## 3. align CRS
# reproject raster to match the coordinate reference system of nc_grid
qf_raster_reprojected <- project(qf_raster, vect(nc_grid), method = "bilinear")

## 4. extract raster values using mean per polygon
# exactextractr handles partial overlaps and weighting
# variable name "dtw" needs to match what is in the tracker on OneDrive
nc_grid$qf <- exact_extract(qf_raster_reprojected, nc_grid, 'mean')

# check the result
summary(nc_grid$qf)
plot(nc_grid['qf'], border = NA)

## 3. align CRS
# reproject raster to match the coordinate reference system of nc_grid
sg_raster_reprojected <- project(sg_raster, vect(nc_grid), method = "bilinear")

## 4. extract raster values using mean per polygon
# exactextractr handles partial overlaps and weighting
# variable name "dtw" needs to match what is in the tracker on OneDrive
nc_grid$sg <- exact_extract(sg_raster_reprojected, nc_grid, 'mean')


# 5. Extract modal category per grid cell
nc_grid$surfgeo <- exact_extract(sg_raster_reprojected, nc_grid, 'mean')

# check the result
summary(nc_grid$sg)
plot(nc_grid['sg'], border = NA)
