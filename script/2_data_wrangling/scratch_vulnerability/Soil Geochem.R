source(here::here("script/2_data_wrangling/helper_functions.R"))

# EXTRACT DRAINAGE
## 1. read in grid shapefile
# read in nc_grid, the common base grid for North Carolina
nc_grid <- st_read("data/output/nc_grid/nc_grid.shp")

soilchem <- read_excel("/Users/ellenwei/ncwell/data/input/Appendix_3a_Ahorizon_18Sept2013-2.xls")

soilchem <- subset(soilchem, StateID =="NC") 

soilchem$CollDate <- NULL

# Step 3: Convert soilchem into a spatial points object using lat/lon
soilchem_sf <- st_as_sf(soilchem, coords = c("Longitude", "Latitude"), crs = 4326)

# Step 4: Transform soilchem points to match the CRS of nc_grid
soilchem_sf <- st_transform(soilchem_sf, crs = st_crs(nc_grid))

# Step 5: Perform a spatial join (adds nc_grid polygon attributes to each soilchem point)
soilchem_joined <- st_join(soilchem_sf, nc_grid, left = TRUE)

ggplot() +
  geom_sf(data = nc_grid, fill = NA, color = "black") +
  geom_sf(data = soilchem_sf, aes(color = A_Quartz), size = 2, alpha = 0.7) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(color = "A_Quartz", title = "Soil Chemistry A_Quartz Values Over Grid")
