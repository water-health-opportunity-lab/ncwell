###############################################################################
# Tables S10–S14 — Selection-bias screening tables for the Supporting Information
#
# This wrapper rebuilds the selection-bias screening tables used in Section 6 of
# supplement.tex:
#   - Table S10: Social-capacity numerical variables
#   - Table S11: Hazard numerical variables
#   - Table S12: Physical-vulnerability numerical variables
#   - Table S13: Hazard categorical variables
#   - Table S14: Physical-vulnerability categorical variables
#
# Workflow:
#   1) Recompute the retained-PC / high-weight-variable screening tables
#   2) Add red-flag markers to the numerical tables
#   3) Optionally rebuild supplement.pdf if pdflatex is available
###############################################################################

message("Step 1/3: Rebuilding base selection-bias screening tables...")
source("selection_bias_high_weight_analysis.R", local = new.env(parent = globalenv()))

message("Step 2/3: Applying mean-difference flags to numerical tables...")
source("selection_bias_meanflag_version.R", local = new.env(parent = globalenv()))

message("Step 3/3: Refreshing supplement.pdf (if pdflatex is available)...")
if (nzchar(Sys.which("pdflatex")) && file.exists("supplement.tex")) {
  system2("pdflatex", c("-interaction=nonstopmode", "supplement.tex"), stdout = TRUE, stderr = TRUE)
  system2("pdflatex", c("-interaction=nonstopmode", "supplement.tex"), stdout = TRUE, stderr = TRUE)
  message("Updated supplement.pdf and Tables S10–S14.")
} else {
  message("pdflatex not found or supplement.tex missing; generated the table .csv/.tex outputs only.")
}
