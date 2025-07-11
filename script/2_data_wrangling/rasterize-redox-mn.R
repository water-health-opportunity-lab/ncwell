# set working directory
setwd('~/Desktop/Research/Contamination risk/')

# load package
library('data.table')
library('raster')
library('terra')
# devtools::install_github("dcooley/sfheaders")
library('sfheaders')

################################# Redox condition

# read data
data <- fread('redox10_grid_prediction.csv')


# data
x <- data$x_map
y <- data$y_map
z <- data$pred_pOxic_dbw5

# Create a data.frame of points
points_df <- data.frame(x = x, y = y, z = z)

# Convert to SpatialPointsDataFrame
coordinates(points_df) <- ~x + y

# Convert to raster (define resolution or use rasterFromXYZ)
r <- rasterFromXYZ(as.data.frame(points_df))


## finding coordinate reference system
x_range <- range(x)  # example
y_range <- range(y)


# Create a bounding box with projected coordinates
bbox_proj <- st_sfc(st_polygon(list(rbind(
  c(x_range[1], y_range[1]),
  c(x_range[2], y_range[1]),
  c(x_range[2], y_range[2]),
  c(x_range[1], y_range[2]),
  c(x_range[1], y_range[1])
))), crs = 5070)  # Try EPSG:5070

# Transform to geographic (WGS84)
bbox_latlon <- st_transform(bbox_proj, crs = 4326)
plot(bbox_latlon)

writeRaster(r, "redox.tiff", overwrite = TRUE)

################################# probability of Manganese

# read data
data <- fread('mn10_grid_prediction.csv')


# data
x <- data$x_map
y <- data$y_map
z <- data$pred_pMn50_dbw5

# Create a data.frame of points
points_df <- data.frame(x = x, y = y, z = z)

# Convert to SpatialPointsDataFrame
coordinates(points_df) <- ~x + y

# Convert to raster (define resolution or use rasterFromXYZ)
r <- rasterFromXYZ(as.data.frame(points_df))


writeRaster(r, "mn.tiff", overwrite = TRUE)