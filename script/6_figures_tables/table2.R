###############################################################################
# Table 2 — Descriptive statistics of composite risk scores (25 western counties)
# Manuscript: Hybrid supervised–unsupervised modeling for post-hurricane private
# well contamination risk score using empirical validation and community groundtruthing
#
# Table caption:
# "Descriptive statistics of composite risk scores over the 25 western counties.
#  The table summarizes the mean, standard deviation (SD), median, and interquartile
#  range (25th percentile to 75th percentile) of the composite risk score."
###############################################################################

# ---------------------------
# 0) Packages
# ---------------------------
suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(flextable)
  library(officer)
})

# ---------------------------
# 1) Paths (avoid setwd() for reviewer-friendly code)
# ---------------------------
data_dir <- "~/Desktop/Research/Contamination risk"

cap_path <- file.path(data_dir, "index_capacity.gpkg")
vul_path <- file.path(data_dir, "index_vulnerability.gpkg")
haz_path <- file.path(data_dir, "index_hazard.gpkg")

out_gpkg <- file.path(data_dir, "nc_grid_index.gpkg")
out_docx <- file.path(data_dir, "Table2_risk_summary_2col.docx")

# ---------------------------
# 2) Read data
# ---------------------------
index_capacity      <- st_read(cap_path, quiet = TRUE)
index_vulnerability <- st_read(vul_path, quiet = TRUE)
index_hazard        <- st_read(haz_path, quiet = TRUE)

# ---------------------------
# 3) Standardize key column names (matches your original positions)
# ---------------------------
names(index_capacity)[4]      <- "index_capacity"
names(index_hazard)[4]        <- "index_hazard"
names(index_vulnerability)[3] <- "index_vulnerability"

# ---------------------------
# 4) Merge indices (keep geometry from vulnerability)
# ---------------------------
index <- index_vulnerability %>%
  left_join(st_drop_geometry(index_hazard),   by = "grid_id") %>%
  left_join(st_drop_geometry(index_capacity), by = "grid_id")

# ---------------------------
# 5) Resolve duplicated block group name columns (.x/.y)
# ---------------------------
if ("block_group_name.x" %in% names(index) && "block_group_name.y" %in% names(index)) {
  index <- index %>%
    mutate(block_group_name = coalesce(block_group_name.x, block_group_name.y)) %>%
    select(-block_group_name.x, -block_group_name.y)
} else if ("block_group_name.x" %in% names(index)) {
  index <- index %>% rename(block_group_name = block_group_name.x)
} else if ("block_group_name.y" %in% names(index)) {
  index <- index %>% rename(block_group_name = block_group_name.y)
} else {
  stop("No block group name column found (expected block_group_name.x or block_group_name.y).")
}

# ---------------------------
# 6) Composite risk score (vulnerability + hazard − capacity), min–max scaled
#    Only compute where capacity exists (consistent with your original logic).
# ---------------------------
index <- index %>%
  mutate(risk_raw = if_else(
    !is.na(index_capacity),
    index_vulnerability + index_hazard - index_capacity,
    NA_real_
  ))

rng <- range(index$risk_raw, na.rm = TRUE)

index <- index %>%
  mutate(risk = case_when(
    is.na(risk_raw)        ~ NA_real_,
    !is.finite(rng[1])     ~ NA_real_,
    !is.finite(rng[2])     ~ NA_real_,
    rng[2] - rng[1] == 0   ~ 0.5,  # degenerate case
    TRUE ~ (risk_raw - rng[1]) / (rng[2] - rng[1])
  ))

# Optional: write the grid with risk score
st_write(index, out_gpkg, layer = "nc_grid_index", driver = "GPKG", append = FALSE)

# ---------------------------
# 7) Target counties (western NC)
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

# ---------------------------
# 8) Subset grid cells to target counties
# ---------------------------
index_subset <- index %>%
  mutate(
    county_raw  = str_extract(block_group_name, "[A-Za-z .'-]+ County"),
    county_norm = str_squish(str_to_title(county_raw))
  ) %>%
  filter(!is.na(county_norm), county_norm %in% target_counties)

# ---------------------------
# 9) County summary + formatted strings
#     - Mean ± SD
#     - Median [IQR]
# ---------------------------
risk_summary <- index_subset %>%
  st_drop_geometry() %>%
  group_by(county_norm) %>%
  summarise(
    Mean   = mean(risk, na.rm = TRUE),
    SD     = sd(risk, na.rm = TRUE),
    Q25    = as.numeric(quantile(risk, 0.25, na.rm = TRUE)),
    Median = median(risk, na.rm = TRUE),
    Q75    = as.numeric(quantile(risk, 0.75, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    # remove " County" for compact display (matches your example)
    County = str_remove(county_norm, "\\s+County$"),
    MeanSD = sprintf("%.2f \u00B1 %.2f", Mean, SD),
    MedIQR = sprintf("%.2f [%.2f\u2013%.2f]", Median, Q25, Q75)
  ) %>%
  select(County, MeanSD, MedIQR) %>%
  arrange(County)

# ---------------------------
# 10) Convert to 2-column layout (two counties per row)
# ---------------------------
n <- nrow(risk_summary)
n_left <- ceiling(n / 2)

left_tbl <- risk_summary[1:n_left, ] %>%
  mutate(row_id = row_number()) %>%
  rename(
    County_L = County,
    MeanSD_L = MeanSD,
    MedIQR_L = MedIQR
  )

right_tbl <- risk_summary[(n_left + 1):n, , drop = FALSE] %>%
  mutate(row_id = row_number()) %>%
  rename(
    County_R = County,
    MeanSD_R = MeanSD,
    MedIQR_R = MedIQR
  )

table2_2col <- left_tbl %>%
  full_join(right_tbl, by = "row_id") %>%
  select(
    County_L, MeanSD_L, MedIQR_L,
    County_R, MeanSD_R, MedIQR_R
  )

# ---------------------------
# 11) Export to Word (flextable) in your exact layout
# ---------------------------
table_caption <- paste0(
  "Table 2. Descriptive statistics of composite risk scores over the 25 western counties. ",
  "The table summarizes the mean, standard deviation (SD), median, and interquartile range ",
  "(25th percentile to 75th percentile) of the composite risk score."
)

ft <- flextable(table2_2col) %>%
  set_header_labels(
    County_L = "County", MeanSD_L = "Mean \u00B1 SD", MedIQR_L = "Median [IQR]",
    County_R = "County", MeanSD_R = "Mean \u00B1 SD", MedIQR_R = "Median [IQR]"
  ) %>%
  set_caption(caption = table_caption) %>%
  autofit()

doc <- read_docx() %>%
  body_add_flextable(ft) %>%
  body_add_par("", style = "Normal")

print(doc, target = out_docx)

message("Saved Word table: ", out_docx)
message("Saved GPKG with risk: ", out_gpkg)
