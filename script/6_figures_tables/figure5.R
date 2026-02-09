###############################################################################
# Figure 5 — Composite risk score + total coliform well-test results
# Manuscript: Hybrid supervised–unsupervised modeling for post-hurricane private
# well contamination risk score using empirical validation and community groundtruthing
#
# Caption:
# Spatial distribution of composite risk scores across western North Carolina,
# overlaid with total coliform well-test results from the testing dataset.
# Darker purple shading denotes higher risk.
# (Top) Green circles represent uncontaminated wells.
# (Bottom) Red crosses represent contaminated wells.
###############################################################################

# ---------------------------
# 0) Packages
# ---------------------------
suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(ggnewscale)
  library(tigris)
  library(ragg)
})

options(tigris_use_cache = TRUE)

# ---------------------------
# 1) Paths & I/O
# ---------------------------
data_dir <- "~/Desktop/Research/Contamination risk"  # <-- edit if needed

index_fp <- file.path(data_dir, "nc_grid_index.gpkg")
wells_fp <- file.path(data_dir, "well_tests.gpkg")

index <- st_read(index_fp, quiet = TRUE)
well_tests <- st_read(wells_fp, quiet = TRUE)

# ---------------------------
# 2) Target counties
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

# County boundaries (NC)
NC_counties <- tigris::counties(state = "NC", year = 2024, class = "sf")
western_counties <- NC_counties %>%
  filter(NAMELSAD %in% target_counties)

# ---------------------------
# 3) Harmonize CRS
# ---------------------------
crs_plot <- 4326
index <- st_transform(index, crs_plot)
well_tests <- st_transform(well_tests, crs_plot)
western_counties <- st_transform(western_counties, crs_plot)

# ---------------------------
# 4) Subset index to target counties
# ---------------------------
index_subset <- index %>%
  mutate(
    county_raw  = str_extract(block_group_name, "[A-Za-z .'-]+ County"),
    county_norm = str_squish(str_to_title(county_raw))
  ) %>%
  filter(!is.na(county_norm) & county_norm %in% target_counties)

# ---------------------------
# 5) Filter wells to study region & add lon/lat
# ---------------------------
# Keep your original filter rule: longitude <= -80
coords <- st_coordinates(st_geometry(well_tests))
well_tests <- well_tests %>%
  mutate(lon = coords[, 1], lat = coords[, 2]) %>%
  filter(lon <= -80)

# Ensure the binary result column is treated consistently (strings)
well_tests <- well_tests %>%
  mutate(Coliform_Result_Binary = as.character(Coliform_Result_Binary))

# ---------------------------
# 6) Plot helper
# ---------------------------
make_risk_overlay_plot <- function(wells_sf,
                                   wells_filter_value,
                                   wells_color,
                                   wells_shape,
                                   wells_size,
                                   wells_alpha = 1,
                                   panel_title = "(a) Composite Risk Score") {
  
  idx_na <- index_subset %>% filter(is.na(risk))
  idx_ok <- index_subset %>% filter(!is.na(risk))
  
  ggplot() +
    # NA polygons (areas not relying on private wells / unavailable risk)
    geom_sf(
      data = idx_na,
      aes(fill = "NA"),
      color = NA,
      show.legend = nrow(idx_na) > 0
    ) +
    scale_fill_manual(
      name   = NULL,
      values = c("NA" = "grey85"),
      breaks = "NA",
      guide  = guide_legend(
        order = 1,
        override.aes = list(color = "grey60")
      )
    ) +
    ggnewscale::new_scale_fill() +
    
    # Non-NA risk polygons (gradient)
    geom_sf(data = idx_ok, aes(fill = risk), color = NA) +
    scale_fill_gradientn(
      colours  = c("white", "pink", "purple", "black"),
      name     = NULL,
      na.value = NA,
      guide    = guide_colourbar(order = 2)
    ) +
    
    # County boundaries
    geom_sf(data = western_counties, fill = NA, color = "black", linewidth = 0.5) +
    
    # Well test results (overlay)
    geom_sf(
      data  = wells_sf %>% filter(Coliform_Result_Binary == wells_filter_value),
      color = wells_color,
      shape = wells_shape,
      size  = wells_size,
      alpha = wells_alpha
    ) +
    
    # Labels & theme
    theme_bw() +
    labs(
      title = panel_title,
      x = bquote("Longitude (" * degree * "W)"),
      y = bquote("Latitude (" * degree * "N)")
    ) +
    theme(
      panel.grid  = element_blank(),
      axis.title  = element_text(size = 14, face = "bold"),
      axis.text   = element_text(size = 12, face = "bold"),
      plot.title  = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.margin = margin(t = 15, r = 15, b = 15, l = 15),
      legend.text = element_text(size = 12)
    )
}

# ---------------------------
# 7) TOP panel: uncontaminated (Absent) — green circles
# ---------------------------
p_absent <- make_risk_overlay_plot(
  wells_sf            = well_tests,
  wells_filter_value  = "Absent",
  wells_color         = "green",
  wells_shape         = 19,
  wells_size          = 3.5,
  wells_alpha         = 0.7,
  panel_title         = "(a) Composite Risk Score"
)

ragg::agg_png(
  filename = "risk_uncontaminated_coliform.png",
  width = 2400, height = 1400, units = "px", res = 300
)
print(p_absent)
dev.off()

# ---------------------------
# 8) BOTTOM panel: contaminated (Present) — red crosses
# ---------------------------
p_present <- make_risk_overlay_plot(
  wells_sf            = well_tests,
  wells_filter_value  = "Present",
  wells_color         = "red",
  wells_shape         = 4,     # "x" shape (numeric is safer than 'x')
  wells_size          = 3.5,   # avoid overly large points in print
  wells_alpha         = 1,
  panel_title         = "(b) Composite Risk Score"
)

ragg::agg_png(
  filename = "risk_contaminated_coliform.png",
  width = 2400, height = 1400, units = "px", res = 300
)
print(p_present)
dev.off()
