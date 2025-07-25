# HYDROCLIMATE PREDICTOR GROUP
#   - Recharge (rech)
#   - Temperature (tmean)
#   - Precipitation (ppt)
#   - Base-Flow Index (bfi)
#   - Stream Density (stm_den)
#   - Evapotranspiration (ET)
#   - Quickflow (qf) 

#   - Effective Recharge (effrech)

#   - Discharge (discharge)
#   - HUC-12 (huc12)

# Read in Effective Recharge
effrech <- rast("/Users/ellenwei/ncwell/data/input/Hydroclimate/reitz_hydrology/EffRecharge_0013_v2/0013/RC_eff_0013.tif")

## 3. align CRS
# reproject raster to match the coordinate reference system of nc_grid
effrech_reprojected <- project(effrech, vect(nc_grid), method = "bilinear")

## 4. extract raster values using mean per polygon
# exactextractr handles partial overlaps and weighting
# variable name "rech" needs to match what is in the tracker on OneDrive
nc_grid$effrech <- exact_extract(effrech, nc_grid, 'mean')

# check the result
summary(nc_grid$effrech)
plot(nc_grid['effrech'], border = NA)

# Read in Recharge Raster
rech <- rast("/Users/ellenwei/ncwell/data/input/Hydroclimate/Recharge/rech48grd-2/rech48grd")

## 3. align CRS
# reproject raster to match the coordinate reference system of nc_grid
rech_reprojected <- project(rech, vect(nc_grid), method = "bilinear")

## 4. extract raster values using mean per polygon
# exactextractr handles partial overlaps and weighting
# variable name "rech" needs to match what is in the tracker on OneDrive
nc_grid$recharge <- exact_extract(rech_reprojected, nc_grid, 'mean')

# check the result
summary(nc_grid$recharge)
plot(nc_grid['recharge'], border = NA)

## Read in Temperature and Precipitation
tmean <- rast ("/Users/ellenwei/ncwell/data/input/Hydroclimate/Temperature/PRISM_tmean_30yr_normal_800mM2_annual_asc.asc")
ppt <- rast ("/Users/ellenwei/ncwell/data/input/Hydroclimate/Precipitation/PRISM_ppt_30yr_normal_800mM2_annual_asc.asc")

## 3. align CRS
# reproject raster to match the coordinate reference system of nc_grid
tmean_reprojected <- project(tmean, vect(nc_grid), method = "bilinear")

## 3. align CRS
# reproject raster to match the coordinate reference system of nc_grid
ppt_reprojected <- project(ppt, vect(nc_grid), method = "bilinear")

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

# BFI
## 2. read in dtw raster
bfi_raster <- rast("/Users/ellenwei/ncwell/data/input/Hydroclimate/BFI/bfi48grd")

## 3. align CRS
# reproject raster to match the coordinate reference system of nc_grid
bfi_raster_reprojected <- project(bfi_raster, vect(nc_grid), method = "bilinear")

## 4. extract raster values using mean per polygon
# exactextractr handles partial overlaps and weighting
# variable name "bfi" needs to match what is in the tracker on OneDrive
nc_grid$bfi <- exact_extract(bfi_raster_reprojected, nc_grid, 'mean')

# check the result
summary(nc_grid$bfi)
plot(nc_grid['bfi'], border = NA)

### STREAM DENSITY
# Read in stream density
stm_den <- rast("/Users/ellenwei/ncwell/data/input/Hydroclimate/Stream Density/stmdenhuc12")

## 3. align CRS
# reproject raster to match the coordinate reference system of nc_grid
stm_den_reprojected <- project(stm_den, vect(nc_grid), method = "bilinear")

## 4. extract raster values using mean per polygon
# exactextractr handles partial overlaps and weighting
# variable name "stm_den" needs to match what is in the tracker on OneDrive
nc_grid$stm_den <- exact_extract(stm_den_reprojected, nc_grid, 'mean')

# check the result
summary(nc_grid$stm_den)
plot(nc_grid['stm_den'], border = NA)

# Read in Evapotranspiration (ET)
ET <- rast("/Users/ellenwei/ncwell/data/input/Hydroclimate/reitz_hydrology/ET_0013/ET_0013.tif")

## 3. align CRS
# reproject raster to match the coordinate reference system of nc_grid
ET_reprojected <- project(ET, vect(nc_grid), method = "bilinear")

## 4. extract raster values using mean per polygon
# exactextractr handles partial overlaps and weighting
# variable name "ET" needs to match what is in the tracker on OneDrive
nc_grid$ET <- exact_extract(ET_reprojected, nc_grid, 'mean')

# check the result
summary(nc_grid$ET)
plot(nc_grid['ET'], border = NA)

#Read in Quickflow (QF)
qf <- rast("/Users/ellenwei/ncwell/data/input/Hydroclimate/reitz_hydrology/QuickFlow_0013/RO_0013.tif")

## 3. align CRS
# reproject raster to match the coordinate reference system of nc_grid
qf_reprojected <- project(qf, vect(nc_grid), method = "bilinear")

## 4. extract raster values using mean per polygon
# exactextractr handles partial overlaps and weighting
# variable name "ET" needs to match what is in the tracker on OneDrive
nc_grid$qf <- exact_extract(qf_reprojected, nc_grid, 'mean')

# check the result
summary(nc_grid$qf)
plot(nc_grid['qf'], border = NA)

#WRITE OUT RESULTS 
st_write(nc_grid, "data/output/nc_grid_vulnerability.gpkg", layer = "vulnerability", delete_layer = TRUE)