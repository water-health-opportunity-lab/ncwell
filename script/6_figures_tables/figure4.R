###############################################################################
# Figure 4. Spatial distribution of the hazard index, physical vulnerability
# index, social capacity index, and composite risk score across 25 counties
# in western North Carolina. Regions shown in gray indicate areas that do not
# rely on private wells. The boundaries of Avery and Henderson Counties, where
# ground-truthing was conducted, are emphasized with bold, teal outlines.
#
# Manuscript:
# Hybrid Supervised–Unsupervised Modeling for Post-Hurricane Private Well
# Contamination Risk Score Using Empirical Validation and Community
# Groundtruthing
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
  library(grid)    # unit()
  library(ragg)    # export
})

options(tigris_use_cache = TRUE)

# ---------------------------
# 1) Paths (avoid setwd() for reproducibility)
# ---------------------------
proj_dir <- "~/Desktop/Research/Contamination risk"
stopifnot(dir.exists(proj_dir))

in_cap <- file.path(proj_dir, "index_capacity.gpkg")
in_vul <- file.path(proj_dir, "index_vulnerability.gpkg")
in_haz <- file.path(proj_dir, "index_hazard.gpkg")

stopifnot(file.exists(in_cap), file.exists(in_vul), file.exists(in_haz))

# ---------------------------
# 2) Read data
# ---------------------------
index_capacity      <- st_read(in_cap, quiet = TRUE)
index_vulnerability <- st_read(in_vul, quiet = TRUE)
index_hazard        <- st_read(in_haz, quiet = TRUE)

# ---------------------------
# 3) Harmonize index column names (safer than position-based renaming)
# ---------------------------
rename_index_col <- function(x, new_name, keep = c("grid_id", "geometry")) {
  nm <- names(x)
  if (new_name %in% nm) return(x)
  
  cand <- setdiff(nm, keep)
  common <- cand[stringr::str_detect(cand, stringr::regex("index|score", ignore_case = TRUE))]
  idx_col <- if (length(common) >= 1) common[1] else cand[1]
  
  x %>% dplyr::rename(!!new_name := !!rlang::sym(idx_col))
}

index_capacity      <- rename_index_col(index_capacity, "index_capacity", keep = c("grid_id", "geometry"))
index_hazard        <- rename_index_col(index_hazard, "index_hazard", keep = c("grid_id", "geometry"))
index_vulnerability <- rename_index_col(index_vulnerability, "index_vulnerability", keep = c("grid_id", "geometry"))

# ---------------------------
# 4) Merge into a single sf (geometry kept from vulnerability)
# ---------------------------
index <- index_vulnerability %>%
  left_join(index_hazard %>% st_drop_geometry(),   by = "grid_id") %>%
  left_join(index_capacity %>% st_drop_geometry(), by = "grid_id")

# ---------------------------
# 4b) Standardize block group name after joins (coalesce x/y)
# ---------------------------
if (!("block_group_name.x" %in% names(index)) && !("block_group_name.y" %in% names(index))) {
  stop("Expected block_group_name.x/y after joins. Please confirm input columns and join keys.")
}

index <- index %>%
  mutate(block_group_name = dplyr::coalesce(block_group_name.x, block_group_name.y)) %>%
  select(-dplyr::any_of(c("block_group_name.x", "block_group_name.y")))

if (all(is.na(index$block_group_name))) {
  stop("block_group_name is missing after coalescing block_group_name.x/y.")
}

# ---------------------------
# 5) Composite risk score: risk = vulnerability + hazard - capacity, then min–max
# ---------------------------
index <- index %>%
  mutate(
    risk_raw = index_vulnerability + index_hazard - index_capacity,
    risk = NA_real_
  )

ok <- !is.na(index$index_capacity) & !is.na(index$index_hazard) & !is.na(index$index_vulnerability)
if (any(ok)) {
  rmin <- min(index$risk_raw[ok], na.rm = TRUE)
  rmax <- max(index$risk_raw[ok], na.rm = TRUE)
  if (isTRUE(all.equal(rmin, rmax))) {
    index$risk[ok] <- 0
  } else {
    index$risk[ok] <- (index$risk_raw[ok] - rmin) / (rmax - rmin)
  }
}

# Optional: write merged layer
out_gpkg <- file.path(proj_dir, "nc_grid_index.gpkg")
st_write(index, out_gpkg, layer = "my_layer", driver = "GPKG", append = FALSE, quiet = TRUE)

# ---------------------------
# 6) Target counties (Western NC)
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

NC_counties <- tigris::counties(state = "NC", year = 2024, cb = TRUE)
western_counties <- NC_counties %>% filter(NAMELSAD %in% target_counties)

# ---------------------------
# 7) Subset index to target counties
# ---------------------------
index_subset <- index %>%
  mutate(
    county_raw  = stringr::str_extract(block_group_name, "[A-Za-z .'-]+ County"),
    county_norm = stringr::str_squish(stringr::str_to_title(county_raw))
  ) %>%
  filter(!is.na(county_norm) & county_norm %in% target_counties)

# ---------------------------
# 8) Plot helper for panels (a)-(c): big title + outside legend
# ---------------------------
plot_index_map <- function(sf_data,
                           value_col,
                           title,
                           colours,
                           out_file,
                           western_counties_sf = western_counties,
                           highlight_names = c("Avery", "Henderson"),
                           na_fill = "grey85",
                           legend_pos = c(0.26, 1.02),
                           width_px = 2200,
                           height_px = 1600,
                           res = 300,
                           clip_off = FALSE) {
  
  sf_data$val_for_plot <- sf_data[[value_col]]
  
  idx_na <- sf_data %>% dplyr::filter(is.na(val_for_plot))
  idx_ok <- sf_data %>% dplyr::filter(!is.na(val_for_plot))
  
  p <- ggplot2::ggplot()
  
  # NA layer (discrete legend)
  if (nrow(idx_na) > 0) {
    p <- p +
      ggplot2::geom_sf(
        data = idx_na,
        ggplot2::aes(fill = "NA"),
        color = NA,
        show.legend = TRUE
      ) +
      ggplot2::scale_fill_manual(
        name   = NULL,
        values = c("NA" = na_fill),
        breaks = "NA",
        guide  = ggplot2::guide_legend(
          order = 1,
          override.aes = list(fill = na_fill)
        )
      ) +
      ggnewscale::new_scale_fill()
  } else {
    p <- p + ggnewscale::new_scale_fill()
  }
  
  # Non-NA layer (continuous)
  p <- p +
    ggplot2::geom_sf(
      data  = idx_ok,
      ggplot2::aes(fill = val_for_plot),
      color = NA
    ) +
    ggplot2::scale_fill_gradientn(
      colours  = colours,
      name     = NULL,
      na.value = NA,
      guide    = ggplot2::guide_colourbar(
        order     = 2,
        barwidth  = 18,
        barheight = 0.6
      )
    ) +
    ggplot2::geom_sf(
      data  = western_counties_sf,
      fill  = NA,
      color = "black",
      linewidth = 0.5
    ) +
    ggplot2::geom_sf(
      data = subset(western_counties_sf, NAME %in% highlight_names),
      fill = NA,
      color = "#00ACC1",
      linewidth = 1.8
    ) +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid       = ggplot2::element_blank(),
      axis.text        = ggplot2::element_blank(),
      axis.ticks       = ggplot2::element_blank(),
      axis.title       = ggplot2::element_blank(),
      
      plot.title = ggplot2::element_text(
        size   = 40,
        face   = "bold",
        hjust  = 0,
        margin = ggplot2::margin(t = 5, b = 55, unit = "pt")
      ),
      
      legend.position      = legend_pos,
      legend.justification = c("left", "bottom"),
      legend.direction     = "horizontal",
      legend.box           = "horizontal",
      legend.spacing.x     = grid::unit(0.5, "cm"),
      legend.text          = ggplot2::element_text(size = 22),
      legend.title         = ggplot2::element_text(size = 22, face = "bold"),
      
      plot.margin = ggplot2::margin(t = 10, r = 5, b = 0, l = 5, unit = "pt")
    )
  
  if (clip_off) {
    p <- p + ggplot2::coord_sf(expand = FALSE, clip = "off")
  } else {
    p <- p + ggplot2::coord_sf(expand = FALSE)
  }
  
  ragg::agg_png(
    filename   = file.path(proj_dir, out_file),
    width      = width_px,
    height     = height_px,
    units      = "px",
    res        = res,
    background = "white"
  )
  print(p)
  grDevices::dev.off()
  
  invisible(p)
}

# ---------------------------
# 9) Plot helper for panel (d): match your ORIGINAL risk style
# ---------------------------
plot_risk_map <- function(sf_data,
                          out_file = "risk_dist.png",
                          western_counties_sf = western_counties,
                          highlight_names = c("Avery", "Henderson"),
                          width_px = 2400,
                          height_px = 1400,
                          res = 300) {
  
  idx_na <- subset(sf_data,  is.na(risk))
  idx_ok <- subset(sf_data, !is.na(risk))
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = idx_na,
      ggplot2::aes(fill = "NA"),
      color = NA,
      show.legend = nrow(idx_na) > 0
    ) +
    ggplot2::scale_fill_manual(
      name   = NULL,
      values = c("NA" = "grey85"),
      breaks = "NA",
      guide  = ggplot2::guide_legend(
        order        = 1,
        override.aes = list(color = "grey60")
      )
    ) +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_sf(
      data = idx_ok,
      ggplot2::aes(fill = risk),
      color = NA
    ) +
    ggplot2::scale_fill_gradientn(
      colours  = c("white", "pink", "purple", "black"),
      name     = NULL,
      na.value = NA,
      guide    = ggplot2::guide_colourbar(order = 2)
    ) +
    ggplot2::geom_sf(
      data  = western_counties_sf,
      fill  = NA,
      color = "black",
      linewidth = 0.5
    ) +
    ggplot2::geom_sf(
      data = subset(western_counties_sf, NAME %in% highlight_names),
      fill = NA,
      color = "#00ACC1",
      linewidth = 1.8
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = "(d) Composite Risk Score",
      x     = bquote("Longitude (" * degree * "W)"),
      y     = bquote("Latitude (" * degree * "N)")
    ) +
    ggplot2::theme(
      panel.grid  = ggplot2::element_blank(),
      axis.title  = ggplot2::element_text(size = 14, face = "bold"),
      axis.text   = ggplot2::element_text(size = 12, face = "bold"),
      plot.title  = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
      plot.margin = ggplot2::margin(t = 15, r = 15, b = 15, l = 15)
    )
  
  ragg::agg_png(
    filename = file.path(proj_dir, out_file),
    width  = width_px,
    height = height_px,
    units  = "px",
    res    = res,
    background = "white"
  )
  print(p)
  grDevices::dev.off()
  
  invisible(p)
}

# ---------------------------
# 10) Figure panels
# ---------------------------

# (a) Hazard — plot hazard only where capacity is available
index_subset <- index_subset %>%
  mutate(hazard_for_plot = if_else(is.na(index_capacity), NA_real_, index_hazard))

plot_index_map(
  sf_data   = index_subset,
  value_col = "hazard_for_plot",
  title     = "(a) Hazard",
  colours   = c("white", "red", "darkred"),
  out_file  = "hazard_index_dist.png",
  clip_off  = TRUE
)

# (b) Physical vulnerability — plot vulnerability only where capacity is available
index_subset <- index_subset %>%
  mutate(vulnerability_for_plot = if_else(is.na(index_capacity), NA_real_, index_vulnerability))

plot_index_map(
  sf_data   = index_subset,
  value_col = "vulnerability_for_plot",
  title     = "(b) Physical Vulnerability",
  colours   = c("white", "skyblue", "navy"),
  out_file  = "vulnerability_index_dist.png"
)

# (c) Social capacity
plot_index_map(
  sf_data   = index_subset,
  value_col = "index_capacity",
  title     = "(c) Social Capacity",
  colours   = c("white", "lightgreen", "darkgreen"),
  out_file  = "capacity_index_dist.png"
)

# (d) Composite risk score — use your original style (axes + centered title)
plot_risk_map(
  sf_data   = index_subset,
  out_file  = "risk_dist.png"
)
