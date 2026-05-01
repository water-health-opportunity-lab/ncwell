###############################################################################
# Table S7 — County-wise descriptive statistics by module
#
# Manuscript: Hybrid supervised–unsupervised modeling for post-hurricane private
# well contamination risk score using empirical validation and community groundtruthing
#
# Caption:
# County-wise descriptive statistics by module (Mean, SD, 25th percentile, Median,
# 75th percentile).
#
# Output:
#   - A LaTeX longtable saved to: Table_S7_by_module.tex
#
# Notes:
#   - This script avoids redundant packages, avoids setwd(), and does not write
#     intermediate GPKGs.
#   - Counties are derived from block_group_name (as in your pipeline).
###############################################################################

# ---------------------------
# 0) Packages
# ---------------------------
suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(stringr)
  library(rlang)
})

# ---------------------------
# 1) Paths & read data
# ---------------------------
data_dir <- "~/Desktop/Research/Contamination risk"  # <-- edit if needed
fp <- function(x) file.path(data_dir, x)

index_capacity      <- st_read(fp("index_capacity.gpkg"), quiet = TRUE)
index_hazard        <- st_read(fp("index_hazard.gpkg"), quiet = TRUE)
index_vulnerability <- st_read(fp("index_vulnerability.gpkg"), quiet = TRUE)

# ---------------------------
# 2) Standardize index column names
# ---------------------------
names(index_capacity)[4]      <- "index_capacity"
names(index_hazard)[4]        <- "index_hazard"
names(index_vulnerability)[3] <- "index_vulnerability"

# ---------------------------
# 3) Merge to one sf object (keep geometry from vulnerability)
# ---------------------------
index <- index_vulnerability %>%
  st_drop_geometry() %>%
  left_join(index_hazard %>% st_drop_geometry(),    by = "grid_id") %>%
  left_join(index_capacity %>% st_drop_geometry(),  by = "grid_id") %>%
  left_join(index_vulnerability %>% select(grid_id, geom), by = "grid_id") %>%
  st_as_sf(sf_column_name = "geom")

# Unify block_group_name if joins created suffixes
if (all(c("block_group_name.x", "block_group_name.y") %in% names(index))) {
  index <- index %>%
    mutate(block_group_name = coalesce(block_group_name.x, block_group_name.y)) %>%
    select(-block_group_name.x, -block_group_name.y)
}

stopifnot("block_group_name" %in% names(index))

# ---------------------------
# 4) Target counties (western NC)
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

index_subset <- index %>%
  mutate(
    county_raw  = str_extract(block_group_name, "[A-Za-z .'-]+ County"),
    county_norm = str_squish(str_to_title(county_raw))
  ) %>%
  filter(!is.na(county_norm) & county_norm %in% target_counties)

stopifnot(nrow(index_subset) > 0)

# ---------------------------
# 5) County-wise summary helper
# ---------------------------
summarise_index <- function(data, county_col, value_col) {
  county_sym <- rlang::ensym(county_col)
  value_sym  <- rlang::ensym(value_col)
  
  data %>%
    st_drop_geometry() %>%
    group_by(!!county_sym) %>%
    summarise(
      Mean   = mean(!!value_sym, na.rm = TRUE),
      SD     = sd(!!value_sym, na.rm = TRUE),
      `25%`  = as.numeric(stats::quantile(!!value_sym, 0.25, na.rm = TRUE, names = FALSE)),
      Median = stats::median(!!value_sym, na.rm = TRUE),
      `75%`  = as.numeric(stats::quantile(!!value_sym, 0.75, na.rm = TRUE, names = FALSE)),
      .groups = "drop"
    ) %>%
    arrange(!!county_sym) %>%
    rename(County = !!county_sym) %>%
    mutate(across(where(is.numeric), ~ round(.x, 2)))
}

# ---------------------------
# 6) LaTeX helpers (minimal + robust)
# ---------------------------
escape_latex <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\\\\", "\\\\textbackslash{}")
  x <- stringr::str_replace_all(x, "([#$%&_{}])", "\\\\\\1")
  x <- stringr::str_replace_all(x, "\\^", "\\\\textasciicircum{}")
  x <- stringr::str_replace_all(x, "~", "\\\\textasciitilde{}")
  x
}

df_to_latex_rows <- function(df) {
  fmt <- function(v) ifelse(is.na(v), "", formatC(v, format = "f", digits = 2))
  county <- escape_latex(df$County)
  
  paste0(
    county, " & ",
    fmt(df$Mean),   " & ",
    fmt(df$SD),     " & ",
    fmt(df$`25%`),  " & ",
    fmt(df$Median), " & ",
    fmt(df$`75%`),  " \\\\"
  )
}

make_table_S7_latex <- function(index_subset, file = "Table_S7_by_module.tex") {
  
  # Compute summaries
  haz_tbl <- summarise_index(index_subset, county_norm, index_hazard)
  vul_tbl <- summarise_index(index_subset, county_norm, index_vulnerability)
  cap_tbl <- summarise_index(index_subset, county_norm, index_capacity)
  
  header_top <- paste0(
    "\\begin{longtable}{lrrrrr}
\\caption{County-wise descriptive statistics by module (Mean, SD, 25th percentile, Median, 75th percentile).}\\\\
\\label{tab:S7_by_module}\\\\
\\toprule
\\textbf{County} & \\textbf{Mean} & \\textbf{SD} & \\textbf{25\\%} & \\textbf{Median} & \\textbf{75\\%} \\\\
\\midrule
\\endfirsthead

\\multicolumn{6}{c}{{\\bfseries \\tablename\\ \\thetable{} -- continued from previous page}} \\\\
\\toprule
\\textbf{County} & \\textbf{Mean} & \\textbf{SD} & \\textbf{25\\%} & \\textbf{Median} & \\textbf{75\\%} \\\\
\\midrule
\\endhead

\\midrule
\\multicolumn{6}{r}{{Continued on next page}} \\\\
\\endfoot

\\bottomrule
\\endlastfoot
"
  )
  
  # ---- Panels in requested order ----
  panel_a <- "\\multicolumn{6}{l}{\\textbf{(a) Hazard module}} \\\\\n\\midrule\n"
  panel_b <- "\\addlinespace[6pt]\n\\multicolumn{6}{l}{\\textbf{(b) Physical vulnerability module}} \\\\\n\\midrule\n"
  panel_c <- "\\addlinespace[6pt]\n\\multicolumn{6}{l}{\\textbf{(c) Social capacity module}} \\\\\n\\midrule\n"
  
  body_a <- paste(df_to_latex_rows(haz_tbl), collapse = "\n")
  body_b <- paste(df_to_latex_rows(vul_tbl), collapse = "\n")
  body_c <- paste(df_to_latex_rows(cap_tbl), collapse = "\n")
  
  latex <- paste0(
    header_top,
    panel_a, body_a, "\n",
    panel_b, body_b, "\n",
    panel_c, body_c, "\n",
    "\\end{longtable}\n"
  )
  
  writeLines(latex, file, useBytes = TRUE)
  message("Wrote LaTeX to: ", file)
  latex
}

# ---------------------------
# Generate Table S7
# ---------------------------
latex_code <- make_table_S7_latex(
  index_subset,
  file = fp("Table_S7_by_module.tex")
)

cat(latex_code)

# ---------------------------
# 7) Generate Table S7 LaTeX
# ---------------------------
latex_code <- make_table_S7_latex(index_subset, file = fp("Table_S7_by_module.tex"))
cat(latex_code)
