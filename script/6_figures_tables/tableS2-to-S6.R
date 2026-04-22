###############################################################################
# Tables S2–S6 — Summary statistics for module variables (Supporting Information)
#
# Manuscript: Hybrid supervised–unsupervised modeling for post-hurricane private
# well contamination risk score using empirical validation and community groundtruthing
#
# Key requirements handled:
#   - Use your original summ() function.
#   - Treat integer-valued % variables (e.g., pct_women) as continuous.
#   - Social capacity module variables are all continuous.
#   - Remove grid_id from ALL summaries (prevents the row:
#       grid_id & 72646.09 & 29349.45 & 71993 & 47277-96804
#     from appearing).
###############################################################################

# ---------------------------
# 0) Packages
# ---------------------------
suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(stringr)
  library(readxl)
  library(xtable)
})

# ---------------------------
# 1) Paths
# ---------------------------
data_dir <- "~/Desktop/Research/Contamination risk"  # <-- edit if needed
fp <- function(x) file.path(data_dir, x)

capacity_fp    <- fp("nc_grid_capacity.gpkg")
hazard_fp      <- fp("nc_grid_hazard.gpkg")
vul_raw_fp     <- fp("nc_grid_vulnerability_929.gpkg")
vul_imputed_fp <- fp("nc_grid_vulnerability_imputed.gpkg")
dict_fp        <- fp("Table S1.xlsx")

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

# ---------------------------
# 3) Helper functions (KEEP your summ())
# ---------------------------
iqr <- function(x) {
  paste0(
    round(quantile(na.omit(x), 0.25), 2), "-",
    round(quantile(na.omit(x), 0.75), 2)
  )
}

summ <- function(x, names) {
  p <- ncol(x)
  n <- nrow(x)  # kept for fidelity with your original function
  tab <- data.frame()
  for (j in 1:p) {
    x.j <- x[, j]
    info <- c(
      round(mean(x.j, na.rm = TRUE), 2),
      round(sd(x.j, na.rm = TRUE), 2),
      round(median(x.j, na.rm = TRUE), 2),
      iqr(x.j)
    )
    tab <- rbind(tab, info)
  }
  tab <- cbind(names, tab)
  colnames(tab) <- c("Name", "Mean", "SD", "Median", "IQR")
  print(tab)
  tab
}

# Treat integer as continuous too (fixes pct_women being misclassified)
is_continuous <- function(z) inherits(z, c("numeric", "integer"))

load_unit_dict <- function(dict_fp, sheet) {
  dict <- read_excel(dict_fp, sheet = sheet)
  colnames(dict)[2] <- "Feature_Names"
  dict %>% select(Feature_Names, Unit)
}

attach_units_by_name <- function(summary_df, unit_df) {
  summary_df %>% left_join(unit_df, by = c("Name" = "Feature_Names"))
}

print_categorical_tables <- function(df) {
  if (ncol(df) == 0) return(invisible(NULL))
  for (v in names(df)) {
    cat("\n----------------------------------------\n")
    cat("Categorical variable:", v, "\n")
    print(table(df[[v]], useNA = "ifany"))
  }
}

# ---------------------------
# 4) Load module datasets
# ---------------------------

# ---- Capacity module ----
capacity <- st_read(capacity_fp, quiet = TRUE)

# Keep your original drop-by-position, but guard against column shifts
drop_cap_cols <- c(25, 26, 28, 29)
drop_cap_cols <- drop_cap_cols[drop_cap_cols <= ncol(capacity)]
capacity <- capacity[, -drop_cap_cols, drop = FALSE]

unit_capacity <- load_unit_dict(dict_fp, "Social Capacity")

# ---- Hazard module ----
hazard <- st_read(hazard_fp, quiet = TRUE) %>% st_drop_geometry()
unit_hazard <- load_unit_dict(dict_fp, "Hazard")

# ---- Vulnerability module ----
vul_raw <- st_read(vul_raw_fp, quiet = TRUE)

# Match your original factor handling
vul_raw$drainage_class_int <- as.factor(round(vul_raw$drainage_class_int))
vul_raw$str_int            <- as.factor(round(vul_raw$str_int))
vul_raw$weg_int            <- as.factor(round(vul_raw$weg_int))
vul_raw$hydgrp_int         <- as.factor(round(vul_raw$hydgrp_int))

# Drop A_* columns
vul_raw <- vul_raw %>% select(-matches("^A_"))

# Align variable set with imputed version
vul_imp <- st_read(vul_imputed_fp, quiet = TRUE)
keep_names <- intersect(names(vul_raw), names(vul_imp))

vulnerability <- vul_raw[, keep_names, drop = FALSE] %>%
  st_drop_geometry() %>%
  select(-c(aq_rocktype, weg_int, landuse, drainage_class_int, ID))

unit_vul <- load_unit_dict(dict_fp, "Physical Vulnerability")

# ---------------------------
# 5) Build full dataset & restrict (Pct_Wells == 100; western counties)
# ---------------------------
full <- capacity %>%
  st_drop_geometry() %>%
  left_join(hazard,        by = "grid_id") %>%
  left_join(vulnerability, by = "grid_id") %>%
  select(-any_of(c("ID.x", "ID.y", "ID")))

stopifnot("Pct_Wells" %in% names(full))
full_subset <- full %>% filter(Pct_Wells == 100)

stopifnot("block_group_name" %in% names(full_subset))
full_subset <- full_subset %>%
  mutate(
    county_raw  = str_extract(block_group_name, "[A-Za-z .'-]+ County"),
    county_norm = str_squish(str_to_title(county_raw))
  ) %>%
  filter(!is.na(county_norm) & county_norm %in% target_counties) %>%
  select(-county_raw, -county_norm)

stopifnot(nrow(full_subset) > 0)

# ---------------------------
# 6) Condense selected categorical variables (your original logic)
# ---------------------------
if ("KB" %in% names(full_subset)) {
  KB <- as.character(full_subset$KB)
  top3 <- names(sort(table(KB), decreasing = TRUE))[1:min(3, length(unique(KB)))]
  KB[!(KB %in% top3)] <- "others"
  full_subset$KB <- KB
}

if ("surfgeo" %in% names(full_subset)) {
  sg <- as.character(full_subset$surfgeo)
  top2 <- names(sort(table(sg), decreasing = TRUE))[1:min(2, length(unique(sg)))]
  sg[!(sg %in% top2)] <- "others"
  full_subset$surfgeo <- sg
}

if ("lith" %in% names(full_subset)) {
  lith <- as.character(full_subset$lith)
  top2 <- names(sort(table(lith), decreasing = TRUE))[1:min(2, length(unique(lith)))]
  lith[!(lith %in% top2)] <- "others"
  full_subset$lith <- lith
}

if ("landcover" %in% names(full_subset)) {
  cate <- as.character(round(full_subset$landcover))
  cate.replace <- vapply(cate, function(x) {
    if (x %in% c("21", "22", "23", "24")) return("Developed")
    if (x %in% c("81", "82"))             return("Agriculture")
    if (x %in% c("41", "42", "43"))       return("Forest")
    if (x %in% c("51", "52"))             return("Shrubland")
    if (x %in% c("71", "72"))             return("Grassland")
    if (x %in% "31")                      return("Barren Land")
    if (x %in% c("90", "95"))             return("Wetlands")
    if (x %in% c("11", "12"))             return("Water")
    NA_character_
  }, character(1))
  full_subset$landcover <- cate.replace
}

# ---------------------------
# 7) Tables S2–S6
#    NOTE: Define exact variable lists for S5/S6 if needed.
# ---------------------------

###############################################################################
# Table S2 — Physical Vulnerability module
###############################################################################
vul_vars <- intersect(names(vulnerability), names(full_subset))
vul_df <- full_subset %>%
  select(any_of(vul_vars)) %>%
  select(-any_of("grid_id"))  # REMOVE grid_id from summary

vul_num_idx <- which(vapply(vul_df, is_continuous, logical(1)))
vul_cat_idx <- which(!vapply(vul_df, is_continuous, logical(1)))

if (length(vul_num_idx) > 0) {
  tab_S2 <- summ(vul_df[, vul_num_idx, drop = FALSE],
                 names = colnames(vul_df)[vul_num_idx])
  tab_S2 <- attach_units_by_name(tab_S2, unit_vul)
  
  print(tab_S2)
  print(xtable(tab_S2), type = "latex")
}

if (length(vul_cat_idx) > 0) {
  print_categorical_tables(vul_df[, vul_cat_idx, drop = FALSE])
}

###############################################################################
# Table S3 — Hazard module
###############################################################################
haz_vars <- intersect(names(hazard), names(full_subset))
haz_df <- full_subset %>%
  select(any_of(haz_vars)) %>%
  select(-any_of("grid_id"))  # REMOVE grid_id from summary

haz_num_idx <- which(vapply(haz_df, is_continuous, logical(1)))
haz_cat_idx <- which(!vapply(haz_df, is_continuous, logical(1)))

if (length(haz_num_idx) > 0) {
  tab_S3 <- summ(haz_df[, haz_num_idx, drop = FALSE],
                 names = colnames(haz_df)[haz_num_idx])
  tab_S3 <- attach_units_by_name(tab_S3, unit_hazard)
  
  print(tab_S3)
  print(xtable(tab_S3), type = "latex")
}

# Print ONLY true categorical variables (factor/character/logical);
# integers like pct_women will NOT end up here.
if (length(haz_cat_idx) > 0) {
  print_categorical_tables(haz_df[, haz_cat_idx, drop = FALSE])
}

###############################################################################
# Table S4 — Social Capacity module (ALL continuous; explicit variable list)
###############################################################################

# Explicit Social Capacity variables (add more here if needed)
cap_vars_S4 <- c(
  "pct_poverty",
  "pct_women_gave_birth",
  "pct_insured",
  "pct_us_born",
  "pct_no_move",
  "pct_gov_workers",
  "pct_minority",
  "pct_limited_english",
  "pct_single_parent",
  "pct_unemployed",
  "pct_no_college",
  "pct_women",
  "pct_over_65",
  "pct_under_17",
  "pct_owner_occupied",
  "pct_active_commuting",
  "pct_crowded_housing",
  "pct_plumbing",
  "pct_new_home",
  "pct_no_vehicle",
  "pct_no_internet"
)

# Pull exactly these variables from the analysis dataset
# (full_subset already restricted to Pct_Wells==100 and the 25 counties)
cap_df <- full_subset %>%
  dplyr::select(any_of(cap_vars_S4))

# Sanity checks: report missing variables (won't crash; useful for debugging)
missing_cap <- setdiff(cap_vars_S4, names(cap_df))
if (length(missing_cap) > 0) {
  warning("These Social Capacity variables were not found in full_subset:\n  ",
          paste(missing_cap, collapse = ", "))
}

# Treat numeric + integer as continuous; convert if needed
# (Sometimes % fields are read as integer; summ() is fine with that.)
cap_num_idx <- which(vapply(cap_df, is_continuous, logical(1)))

# If any are not numeric/integer, attempt coercion to numeric (common for % stored as character)
if (length(cap_num_idx) < ncol(cap_df)) {
  nonnum <- setdiff(names(cap_df), names(cap_df)[cap_num_idx])
  for (v in nonnum) {
    cap_df[[v]] <- suppressWarnings(as.numeric(cap_df[[v]]))
  }
  cap_num_idx <- which(vapply(cap_df, is_continuous, logical(1)))
}

# Summarize with your original summ()
tab_S4 <- summ(
  cap_df[, cap_num_idx, drop = FALSE],
  names = colnames(cap_df)[cap_num_idx]
)

# Attach units (from Table S1.xlsx / Social Capacity sheet)
tab_S4 <- attach_units_by_name(tab_S4, unit_capacity)

# Print / export LaTeX
print(tab_S4)
print(xtable(tab_S4), type = "latex")
