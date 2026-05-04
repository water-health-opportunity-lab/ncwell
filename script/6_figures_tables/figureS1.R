###############################################################################
# Figure S1 — Ordinary vs Sign-Constrained PCA Scree Plots
#
# Manuscript: Hybrid supervised–unsupervised modeling for post-hurricane private
# well contamination risk score using empirical validation and community groundtruthing
#
# Caption:
# Comparison of scree plots for ordinary PCA and sign-constrained PCA across the
# hazard, physical vulnerability, and social capacity modules. The curves are
# generally similar across methods, indicating that the variance loss induced by
# sign constraints is modest. The overlap is especially close for the physical
# vulnerability module.
###############################################################################

# This wrapper reuses the full scree-comparison workflow and then compiles the
# combined stand-alone TeX figure to PDF when pdflatex is available.

source("sign_constrained_pca_scree_compare.R")

tex_dir <- "reviewer_sign_constraint_outputs"
tex_file <- "all_modules_scree_comparison.tex"

if (nzchar(Sys.which("pdflatex")) && file.exists(file.path(tex_dir, tex_file))) {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(tex_dir)
  system2("pdflatex", c("-interaction=nonstopmode", tex_file), stdout = TRUE, stderr = TRUE)
  system2("pdflatex", c("-interaction=nonstopmode", tex_file), stdout = TRUE, stderr = TRUE)
}
