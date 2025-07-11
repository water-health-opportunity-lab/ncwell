# HELPER FUNCTION
source(here::here("script/2_data_wrangling/helper_functions.R"))

library(sf)
library(readr)
library(maps)

# READ IN NC GRID
## 1. read in grid shapefile
# read in nc_grid, the common base grid for North Carolina
nc_grid <- st_read("data/output/nc_grid/nc_grid.shp")

# READ IN MN10
mn10 <- read.csv("/Users/ellenwei/ncwell/data/input/Redox/mn10_grid_prediction.csv")
mn10train <- read.csv("/Users/ellenwei/ncwell/data/input/Redox/mn_binary50_train.csv")

# READ IN REDOX10
redox10 <-read.csv("/Users/ellenwei/ncwell/data/input/Redox/redox10_grid_prediction.csv")
redox10train <- read.csv("/Users/ellenwei/ncwell/data/input/Redox/redox_train.csv")

# Step 1: Read the CSV
# Assume your CSV has columns named "x" and "y"
redox10 <- read_csv("/Users/ellenwei/ncwell/data/input/Redox/redox10_grid_prediction.csv")

# Step 2: Convert to sf object
redox10 <- st_as_sf(redox10, coords = c("x_map", "y_map"), crs = 5070)

redox10_projected <- st_transform(x = redox10, 
                                  crs = st_crs(nc_grid))

nc_boundary <- st_as_sf(maps::map("state", "north carolina", plot = FALSE, fill = TRUE)) %>%
  st_transform(crs = 32617) # UTM zone 17N (suitable for NC)

# Ensure both are in the same CRS
st_crs(nc_boundary) == st_crs(redox10_projected)

# Subset points that fall inside the NC polygon
redox10_projected_nc <- redox10_projected[st_within(redox10_projected, nc_boundary, sparse = FALSE), ]

redox10_projected_nc <- st_intersection(redox10_projected, nc_boundary)

# Check the geometry
print(st_geometry(redox10))


