###############################################################################
# Table S8 — Sensitivity analysis for the multiplicative composite-risk formulation
#
# Manuscript: Hybrid supervised–unsupervised modeling for post-hurricane private
# well contamination risk score using empirical validation and community groundtruthing
#
# Output:
#   - Table_S8_multiplicative_sensitivity.tex
#   - Table_S8_multiplicative_sensitivity.csv
#
# This script reproduces the testing-set sensitivity analysis reported in
# validation_v6.qmd for the multiplicative composite-risk formulation:
#
#   Risk_mult = Hazard * Physical Vulnerability * 1 / (Social Capacity + 0.001)
#
# The score is min–max scaled to [0, 1] before comparing detect vs. non-detect
# groups on the testing set.
###############################################################################

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(caret)
})
# change to your working directory accordingly
data_dir <- "~/Desktop/Research/Contamination risk"
fp <- function(x) file.path(data_dir, x)

escape_latex <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([#$%&_{}])", "\\\\\\1", x, perl = TRUE)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  x <- gsub("~", "\\\\textasciitilde{}", x, fixed = TRUE)
  x
}

fmt4 <- function(x) formatC(x, format = "f", digits = 4)

# ---------------------------
# 1) Read data
# ---------------------------
validation_dat <- st_read(fp("validation.gpkg"), quiet = TRUE)
vul_dat <- st_read(fp("index_vulnerability_detailed.gpkg"), quiet = TRUE)
haz_dat <- st_read(fp("index_hazard_detailed.gpkg"), quiet = TRUE)
cap_dat <- st_read(fp("index_capacity_detailed.gpkg"), quiet = TRUE)

validation_dat$coliform <- ifelse(validation_dat$Coliform_Result_Binary == "Present", 1, 0)

vul_tab <- st_drop_geometry(vul_dat) %>% rename(index_vulnerability = index)
haz_tab <- st_drop_geometry(haz_dat) %>% rename(index_hazard = index)
cap_tab <- st_drop_geometry(cap_dat) %>% rename(index_capacity = index)

# ---------------------------
# 2) Train/test split (matches validation_v6.qmd)
# ---------------------------
set.seed(403)
train_idx <- caret::createDataPartition(validation_dat$coliform, p = 0.6, list = FALSE)
test_dat <- validation_dat[-train_idx, , drop = FALSE]

# ---------------------------
# 3) Multiplicative risk score
# ---------------------------
risk_df_mult <- vul_tab %>%
  select(grid_id, index_vulnerability) %>%
  left_join(haz_tab %>% select(grid_id, index_hazard), by = "grid_id") %>%
  left_join(cap_tab %>% select(grid_id, index_capacity), by = "grid_id") %>%
  mutate(
    risk_raw_mult = index_vulnerability * index_hazard * (1 / (index_capacity + 0.001)),
    risk_raw_mult = if_else(is.finite(risk_raw_mult), risk_raw_mult, NA_real_),
    risk_scaled_mult = (risk_raw_mult - min(risk_raw_mult, na.rm = TRUE)) /
      (max(risk_raw_mult, na.rm = TRUE) - min(risk_raw_mult, na.rm = TRUE))
  ) %>%
  select(grid_id, risk_scaled_mult)

merged_data_mult <- validation_dat %>%
  left_join(risk_df_mult, by = "grid_id")

risk_mult_test <- merged_data_mult$risk_scaled_mult[-train_idx]

keep <- !is.na(risk_mult_test) & !is.na(test_dat$coliform)
risk_vec_mult <- risk_mult_test[keep]
group_vec_mult <- as.factor(test_dat$coliform[keep])

risk_0_mult <- risk_vec_mult[group_vec_mult == 0]
risk_1_mult <- risk_vec_mult[group_vec_mult == 1]

wilcox_mult <- wilcox.test(risk_vec_mult ~ group_vec_mult, alternative = "less", exact = FALSE)

out <- data.frame(
  mean_detect = mean(risk_1_mult),
  mean_nondetect = mean(risk_0_mult),
  mean_difference = mean(risk_1_mult) - mean(risk_0_mult),
  wilcox_p_value = unname(wilcox_mult$p.value)
)

write.csv(out, fp("Table_S8_multiplicative_sensitivity.csv"), row.names = FALSE)

# ---------------------------
# 4) Write LaTeX table body for supplement.tex
# ---------------------------
latex_lines <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Sensitivity analysis for the multiplicative composite-risk formulation on the testing set.}",
  "\\label{tab:multiplicative-sensitivity}",
  "\\small",
  "\\begin{tabular}{lrrr}",
  "\\toprule",
  "Mean (detect) & Mean (non-detect) & Difference & Wilcoxon $p$ \\\\",
  "\\midrule",
  paste0(
    fmt4(out$mean_detect), " & ",
    fmt4(out$mean_nondetect), " & ",
    fmt4(out$mean_difference), " & ",
    fmt4(out$wilcox_p_value), " \\\\"
  ),
  "\\bottomrule",
  "\\end{tabular}",
  "\\normalsize",
  "\\end{table}"
)

writeLines(latex_lines, fp("Table_S8_multiplicative_sensitivity.tex"))
message("Wrote LaTeX to: ", fp("Table_S8_multiplicative_sensitivity.tex"))
message("Wrote CSV to: ", fp("Table_S8_multiplicative_sensitivity.csv"))
