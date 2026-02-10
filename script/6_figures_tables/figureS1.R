###############################################################################
# Figure S1 — Histograms of module indices (Supporting Information)
#
# Manuscript: Hybrid supervised–unsupervised modeling for post-hurricane private
# well contamination risk score using empirical validation and community groundtruthing
#
# Caption:
# Histograms of the social capacity, hazard, and physical vulnerability indices
# for the 25 western North Carolina counties, restricted to areas where private
# well usage is 100%.
###############################################################################

# ---------------------------
# 0) Packages
# ---------------------------
suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(stringr)
  library(tigris)
})

options(tigris_use_cache = TRUE)

# ---------------------------
# 1) Paths & read data
# ---------------------------
data_dir <- "~/Desktop/Research/Contamination risk"  # <-- edit if needed

index_capacity_fp      <- file.path(data_dir, "index_capacity.gpkg")
index_vulnerability_fp <- file.path(data_dir, "index_vulnerability.gpkg")
index_hazard_fp        <- file.path(data_dir, "index_hazard.gpkg")

index_capacity      <- st_read(index_capacity_fp, quiet = TRUE)
index_vulnerability <- st_read(index_vulnerability_fp, quiet = TRUE)
index_hazard        <- st_read(index_hazard_fp, quiet = TRUE)

# ---------------------------
# 2) Standardize column names
#    (keeps your original intent but avoids fragile numeric indexing downstream)
# ---------------------------
# index_capacity: 4th column is the capacity index
names(index_capacity)[4] <- "index_capacity"

# index_hazard: 4th column is the hazard index
names(index_hazard)[4] <- "index_hazard"

# index_vulnerability: 3rd column is the vulnerability index
names(index_vulnerability)[3] <- "index_vulnerability"

# ---------------------------
# 3) Merge indices into one sf object
# ---------------------------
# Keep geometry from vulnerability file; join attributes from others by grid_id
index <- index_vulnerability %>%
  st_drop_geometry() %>%
  left_join(index_hazard %>% st_drop_geometry(),    by = "grid_id") %>%
  left_join(index_capacity %>% st_drop_geometry(),  by = "grid_id") %>%
  left_join(index_vulnerability %>% select(grid_id, geom), by = "grid_id") %>%
  st_as_sf(sf_column_name = "geom")

# Create a single block_group_name (joins may create .x / .y suffixes)
# Your `names(index)` shows both block_group_name.x and block_group_name.y exist.
index <- index %>%
  mutate(block_group_name = dplyr::coalesce(block_group_name.x, block_group_name.y)) %>%
  select(-block_group_name.x, -block_group_name.y)

# ---------------------------
# 4) (Optional) Composite risk score (kept for reproducibility)
#     risk = vulnerability + hazard - capacity, normalized to [0, 1]
# ---------------------------
index$risk <- NA_real_
ok <- !is.na(index$index_capacity) & !is.na(index$index_hazard) & !is.na(index$index_vulnerability)

if (any(ok)) {
  index$risk[ok] <- index$index_vulnerability[ok] + index$index_hazard[ok] - index$index_capacity[ok]
  index$risk[ok] <- (index$risk[ok] - min(index$risk[ok])) / (max(index$risk[ok]) - min(index$risk[ok]))
}

# If you truly need to write this out for later scripts, uncomment:
# out_fp <- file.path(data_dir, "nc_grid_index.gpkg")
# st_write(index, out_fp, layer = "nc_grid_index", driver = "GPKG", append = FALSE)

# ---------------------------
# 5) Target counties (western NC)
# ---------------------------
target_counties <- c(
  "Alexander County","Alleghany County","Ashe County","Avery County",
  "Buncombe County","Burke County","Caldwell County","Catawba County",
  "Clay County","Cleveland County","Gaston County","Haywood County",
  "Henderson County","Jackson County","Lincoln County","Macon County",
  "Madison County","Mcdowell County","Mitchell County","Polk County",
  "Rutherford County","Transylvania County","Watauga County","Wilkes County",
  "Yancey County"
)

# Subset to the 25 counties using the block_group_name string (as in your workflow)
index_subset <- index %>%
  mutate(
    county_raw  = str_extract(block_group_name, "[A-Za-z .'-]+ County"),
    county_norm = str_squish(str_to_title(county_raw))
  ) %>%
  filter(!is.na(county_norm) & county_norm %in% target_counties)

stopifnot(nrow(index_subset) > 0)

# NOTE:
# Caption says “restricted to areas where private well usage is 100%”.
# This script assumes that restriction is already encoded in the index layers.
# If needed, you can enforce it here, e.g.:
# index_subset <- index_subset %>% filter(Pct_Wells == 100)

# ---------------------------
# 6) Figure S1 — Histograms (base R)
# ---------------------------
par(
  mfrow    = c(1, 3),
  mar      = c(4.5, 4.5, 1.5, 1.0),
  cex.lab  = 1.2,
  cex.axis = 1.1
)

hist(
  index_subset$index_hazard,
  main   = "",
  xlab   = "(a) Hazard",
  border = "firebrick",
  col    = "white"
)

hist(
  index_subset$index_vulnerability,
  main   = "",
  xlab   = "(b) Physical Vulnerability",
  border = "navy",
  col    = "white"
)

hist(
  index_subset$index_capacity,
  main   = "",
  xlab   = "(c) Social Capacity",
  border = "darkgreen",
  col    = "white"
)
