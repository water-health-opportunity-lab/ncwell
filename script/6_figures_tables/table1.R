###############################################################################
# Table 1 script — PC selection (majority vote) + constrained logistic (λ≈0)
# Manuscript:
# Hybrid Supervised–Unsupervised Modeling for Post-Hurricane
# Private Well Contamination Risk Scores Using Empirical Validation
# and Community Ground-Truthing
# private well contamination risk score
#
# What this script does:
#   (A) Build analysis-ready validation dataset (merge hazard/vulnerability/capacity)
#   (B) Run repeated train/test splits (seeds 1–500) and record which PCs are
#       selected by constrained logistic LASSO within each module (majority vote)
#   (C) Using the fixed split seed = 403, fit constrained logistic regression
#       (glmnet with λ ≈ 0) on the chosen PCs for each module
#   (D) Print Table-1 style output:
#         - coefficients first (no AUC shown)
#         - rounded to 0.01
#         - approximate relative ratio, e.g., -0.01,-0.08,-0.35,-0.12 -> 1:8:35:12
#
# Notes:
#   - This script avoids reporting AUC/ROC (per Table 1 requirement).
#   - Imputation is leakage-safe: medians computed within each split.
#   - Sign constraints:
#       Vulnerability: β ≥ 0
#       Hazard:        β ≥ 0
#       Capacity:      β ≤ 0
###############################################################################

## ================================================================
## 0) Packages
## ================================================================
pkgs <- c("sf", "dplyr", "caret", "glmnet", "pROC", "tidyselect")
missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) {
  stop(
    "Missing packages: ", paste(missing_pkgs, collapse = ", "),
    "\nPlease install them before running (e.g., install.packages(...))."
  )
}

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(caret)
  library(glmnet)
  library(pROC)       # loaded but not used for AUC output; kept for compatibility
  library(tidyselect)
})

## ================================================================
## 1) Read + merge data
## ================================================================
data    <- sf::st_read("validation.gpkg", quiet = TRUE)
vul_dat <- sf::st_read("index_vulnerability_detailed.gpkg", quiet = TRUE)
haz_dat <- sf::st_read("index_hazard_detailed.gpkg", quiet = TRUE)
cap_dat <- sf::st_read("index_capacity_detailed.gpkg", quiet = TRUE)

# Time since reference date
data$time <- as.numeric(data$Date_Collected - as.Date("2024-09-25"))

# Outcome: coliform detected (1) vs not (0)
data$coliform <- ifelse(data$Coliform_Result_Binary == "Present", 1L, 0L)

# Drop geometry from module tables before join
vul_tab <- sf::st_drop_geometry(vul_dat)
haz_tab <- sf::st_drop_geometry(haz_dat)
cap_tab <- sf::st_drop_geometry(cap_dat)

merged_data <- data %>%
  dplyr::left_join(vul_tab, by = "grid_id") %>%
  dplyr::left_join(haz_tab, by = "grid_id") %>%
  dplyr::left_join(cap_tab, by = "grid_id") %>%
  # remove duplicate join suffixes
  dplyr::select(
    -tidyselect::matches("\\.x$"),
    -tidyselect::matches("\\.y$"),
    -tidyselect::matches("\\.x\\.x$"),
    -tidyselect::matches("\\.y\\.y$")
  ) %>%
  # drop obvious non-predictors / outcomes
  dplyr::select(
    -tidyselect::any_of(c(
      "System_Name", "Street_Address", "City", "State", "Zip",
      "Street_Address_Concat", "EIN", "StarLiMS_ID",
      "Date_Collected", "Sampling_Point", "Comment",
      "Accuracy", "Accuracy_Type", "grid_id",
      "index_vulnerability", "index_hazard", "index_capacity",
      "risk", "Total_Coliform_Result", "Ecoli_Result"
    ))
  ) %>%
  # drop geometry if still present
  sf::st_drop_geometry() %>%
  # drop all A_* variables and specific unwanted columns
  dplyr::select(
    -tidyselect::starts_with("A_"),
    -tidyselect::any_of(c(
      "Ecoli_Result_Binary",
      "Coliform_Result_Binary",
      "ecoli",
      "y_fake"
    ))
  )

## ================================================================
## 2) Basic cleanup: remove all-NA / constant columns; coerce types
## ================================================================
is_all_na   <- vapply(merged_data, function(x) all(is.na(x)), logical(1))
is_const    <- vapply(merged_data, function(x) length(unique(na.omit(x))) <= 1, logical(1))
merged_data <- merged_data[, !(is_all_na | is_const), drop = FALSE]

# Characters -> factors; ensure binary 0/1 outcome
merged_data[] <- lapply(merged_data, function(x) if (is.character(x)) factor(x) else x)
stopifnot("coliform" %in% names(merged_data))
merged_data$coliform <- as.integer(merged_data$coliform == 1L)
stopifnot(length(unique(merged_data$coliform)) == 2L)

## ================================================================
## 3) Imputation helpers (leakage-safe)
## ================================================================
impute_full <- function(df, vars, outcome = "coliform") {
  out <- df[, c(outcome, intersect(vars, names(df))), drop = FALSE]
  out[] <- lapply(out, function(x) if (is.character(x)) factor(x) else x)
  
  for (v in setdiff(names(out), outcome)) {
    if (is.numeric(out[[v]])) {
      med <- suppressWarnings(stats::median(out[[v]], na.rm = TRUE))
      if (!is.finite(med)) med <- 0
      out[[v]][is.na(out[[v]])] <- med
    } else {
      if (!is.factor(out[[v]])) out[[v]] <- factor(out[[v]])
      lev <- levels(out[[v]])
      if (!("Missing" %in% lev)) lev <- c(lev, "Missing")
      out[[v]] <- factor(out[[v]], levels = lev)
      out[[v]][is.na(out[[v]])] <- "Missing"
    }
  }
  out
}

impute_numeric_median <- function(df) {
  out <- df
  for (v in names(out)) {
    if (is.numeric(out[[v]])) {
      med <- suppressWarnings(stats::median(out[[v]], na.rm = TRUE))
      if (!is.finite(med)) med <- 0
      out[[v]][is.na(out[[v]])] <- med
    }
  }
  out
}

## ================================================================
## 4) PC name utilities
## ================================================================
# Extract actual column names from df that match prefix + PC indices
get_pc_vars <- function(prefix, pc_index_vec, df) {
  all_names <- names(df)
  target_names <- paste0(prefix, "_PC", pc_index_vec)
  idx <- which(tolower(all_names) %in% tolower(target_names))
  all_names[idx]
}

# Count nonzero coefficients excluding intercept
count_nonzero <- function(beta_mat) {
  sum(beta_mat[rownames(beta_mat) != "(Intercept)", 1] != 0)
}

## ================================================================
## 5) Module LASSO (selection only; constrained)
## ================================================================
fit_lasso_module_pc_only <- function(
    df, module_vars, outcome = "coliform",
    pc_pattern = "PC", ignore_case = FALSE,
    module_name = "module",
    nfolds = 10,
    lower.limits = -Inf, upper.limits = Inf,
    dfmax = NULL, pmax = NULL,
    penalty.factor = NULL,
    use_1se = TRUE,
    target_k = NULL
) {
  stopifnot(outcome %in% names(df))
  
  keep <- intersect(module_vars, names(df))
  keep <- setdiff(keep, outcome)
  keep <- keep[grepl(pc_pattern, keep, ignore.case = ignore_case)]
  if (length(keep) == 0L) {
    message(sprintf("[%s] No PC variables found; skipping.", module_name))
    return(NULL)
  }
  
  dat_mod <- impute_full(df, keep, outcome = outcome)
  
  # Design matrix; drop intercept
  X <- model.matrix(stats::as.formula(paste(outcome, "~ .")), data = dat_mod)[, -1, drop = FALSE]
  y <- dat_mod[[outcome]]
  
  # Drop constant columns
  is_constX <- vapply(seq_len(ncol(X)), function(j) length(unique(X[, j])) <= 1, logical(1))
  if (any(is_constX)) X <- X[, !is_constX, drop = FALSE]
  if (ncol(X) == 0L) {
    message(sprintf("[%s] No nonconstant predictors; skipping.", module_name))
    return(NULL)
  }
  
  # Align penalty.factor to colnames(X)
  if (is.null(penalty.factor)) {
    penalty.factor <- rep(1, ncol(X))
  } else {
    pf <- penalty.factor
    if (!is.null(names(pf))) {
      pf_aligned <- pf[colnames(X)]
      pf_aligned[is.na(pf_aligned)] <- 1
      penalty.factor <- as.numeric(pf_aligned)
    } else if (length(pf) == ncol(X)) {
      penalty.factor <- as.numeric(pf)
    } else {
      penalty.factor <- rep(1, ncol(X))
    }
  }
  
  set.seed(666)
  cvfit <- cv.glmnet(
    x = X, y = y, family = "binomial",
    alpha = 1, standardize = TRUE, nfolds = nfolds,
    type.measure = "auc",                 # selection criterion only; not printed
    lower.limits = lower.limits, upper.limits = upper.limits,
    dfmax = dfmax, pmax = pmax,
    penalty.factor = penalty.factor
  )
  
  lam_base <- if (use_1se && !is.null(cvfit$lambda.1se)) cvfit$lambda.1se else cvfit$lambda.min
  
  # Optional: choose λ giving ~target_k nonzeros
  if (!is.null(target_k)) {
    all_lam <- sort(unique(c(cvfit$lambda, lam_base)), decreasing = TRUE) # larger λ -> sparser
    nz_count <- vapply(all_lam, function(s) {
      b <- as.matrix(coef(cvfit$glmnet.fit, s = s))
      count_nonzero(b)
    }, integer(1))
    lam_base <- all_lam[which.min(abs(nz_count - target_k))]
  }
  
  bb <- as.matrix(coef(cvfit, s = lam_base))
  nz <- bb[rownames(bb) != "(Intercept)" & as.vector(bb) != 0, , drop = FALSE]
  nz <- nz[order(abs(nz[, 1]), decreasing = TRUE), , drop = FALSE]
  
  list(
    module     = module_name,
    cvfit      = cvfit,
    X          = X,
    y          = y,
    lambda_sel = lam_base,
    coef_best  = nz
  )
}

# If CV-selected λ yields all-zero, extend path and pick first λ with any nonzero
pick_lambda_per_module <- function(
    res,
    lower.limits,
    upper.limits,
    nlambda_extend = 400,
    lambda_min_ratio_extend = 1e-6
) {
  if (is.null(res)) return(NULL)
  if (!is.null(res$coef_best) && nrow(res$coef_best) > 0) return(res)
  
  fit_ext <- glmnet(
    x = res$X, y = res$y,
    family = "binomial", alpha = 1, standardize = TRUE,
    nlambda = nlambda_extend,
    lambda.min.ratio = lambda_min_ratio_extend,
    lower.limits = lower.limits,
    upper.limits = upper.limits
  )
  
  lam_best <- NA_real_
  nz <- NULL
  for (lam in sort(fit_ext$lambda, decreasing = FALSE)) { # small -> less penalty
    b <- as.matrix(coef(fit_ext, s = lam))
    b <- b[rownames(b) != "(Intercept)" & as.vector(b) != 0, , drop = FALSE]
    if (nrow(b) > 0) {
      lam_best <- lam
      nz <- b
      break
    }
  }
  
  if (is.na(lam_best)) {
    message(sprintf("[%s] No nonzero PCs even after extension.", res$module))
    res$lambda_sel <- NA_real_
    res$coef_best  <- NULL
    return(res)
  }
  
  nz <- nz[order(abs(nz[, 1]), decreasing = TRUE), , drop = FALSE]
  res$lambda_sel <- lam_best
  res$coef_best  <- nz
  res
}

## ================================================================
## 6) Repeated splits: seeds 1–500 (PC selection frequencies)
## ================================================================
seeds <- 1:500

sel_vul_list <- vector("list", length(seeds)); names(sel_vul_list) <- as.character(seeds)
sel_haz_list <- vector("list", length(seeds)); names(sel_haz_list) <- as.character(seeds)
sel_cap_list <- vector("list", length(seeds)); names(sel_cap_list) <- as.character(seeds)

# Module variable lists from the detailed tables (preferred)
vars_vul_all <- setdiff(names(vul_tab), "grid_id")
vars_haz_all <- setdiff(names(haz_tab), "grid_id")
vars_cap_all <- setdiff(names(cap_tab), "grid_id")

for (i in seq_along(seeds)) {
  s <- seeds[i]
  cat("Running seed =", s, "\n")
  
  set.seed(s)
  idx <- caret::createDataPartition(merged_data$coliform, p = 0.6, list = FALSE)
  train_dat <- merged_data[idx, , drop = FALSE]
  
  ## Vulnerability: β ≥ 0
  # penalty factor example (optional; aligned safely inside)
  vul_names <- intersect(vars_vul_all, names(train_dat))
  pc_id_raw <- suppressWarnings(as.integer(sub(".*PC(\\d+).*", "\\1", vul_names)))
  pf_vul <- if (all(!is.na(pc_id_raw)) && length(pc_id_raw) > 0L) {
    lo <- min(pc_id_raw); hi <- max(pc_id_raw)
    1 + (pc_id_raw - lo) / (max(1e-8, hi - lo))
  } else rep(1, length(vul_names))
  if (length(pf_vul) > 0L) names(pf_vul) <- vul_names
  
  res_vul <- fit_lasso_module_pc_only(
    df = train_dat,
    module_vars = vars_vul_all,
    module_name = "Vulnerability",
    lower.limits = 0, upper.limits = Inf,
    dfmax = 5, pmax = 29,
    penalty.factor = pf_vul,
    use_1se = TRUE, target_k = 5
  )
  res_vul <- pick_lambda_per_module(res_vul, lower.limits = 0, upper.limits = Inf)
  
  ## Hazard: β ≥ 0
  res_haz <- fit_lasso_module_pc_only(
    df = train_dat,
    module_vars = vars_haz_all,
    module_name = "Hazard",
    lower.limits = 0, upper.limits = Inf,
    dfmax = 5, pmax = 6,
    use_1se = TRUE, target_k = 5
  )
  res_haz <- pick_lambda_per_module(res_haz, lower.limits = 0, upper.limits = Inf)
  
  ## Capacity: β ≤ 0
  res_cap <- fit_lasso_module_pc_only(
    df = train_dat,
    module_vars = vars_cap_all,
    module_name = "Capacity",
    lower.limits = -Inf, upper.limits = 0,
    dfmax = 5, pmax = 10,
    use_1se = TRUE, target_k = 5
  )
  res_cap <- pick_lambda_per_module(res_cap, lower.limits = -Inf, upper.limits = 0)
  
  sel_vul_list[[i]] <- if (!is.null(res_vul$coef_best)) rownames(res_vul$coef_best) else character(0)
  sel_haz_list[[i]] <- if (!is.null(res_haz$coef_best)) rownames(res_haz$coef_best) else character(0)
  sel_cap_list[[i]] <- if (!is.null(res_cap$coef_best)) rownames(res_cap$coef_best) else character(0)
}

make_vote_table <- function(sel_list, seeds) {
  pcs <- unlist(sel_list, use.names = FALSE)
  if (length(pcs) == 0L) {
    return(data.frame(PC = character(0), count = integer(0), prop = numeric(0)))
  }
  tab <- sort(table(pcs), decreasing = TRUE)
  data.frame(
    PC    = names(tab),
    count = as.integer(tab),
    prop  = as.integer(tab) / length(seeds),
    row.names = NULL
  )
}

vote_vul <- make_vote_table(sel_vul_list, seeds)
vote_haz <- make_vote_table(sel_haz_list, seeds)
vote_cap <- make_vote_table(sel_cap_list, seeds)

maj_thresh <- length(seeds) / 2
maj_vul <- subset(vote_vul, count > maj_thresh)
maj_haz <- subset(vote_haz, count > maj_thresh)
maj_cap <- subset(vote_cap, count > maj_thresh)

cat("\n=== Majority PCs (selected in > 50% of runs) ===\n")
cat("\n[Vulnerability]\n"); print(maj_vul)
cat("\n[Hazard]\n");        print(maj_haz)
cat("\n[Capacity]\n");      print(maj_cap)

## ================================================================
## 7) Table-1 formatting: coefficients + rounded + relative ratio
## ================================================================
make_coef_table <- function(coef_mat, digits = 2) {
  # drop intercept
  coefs <- coef_mat[rownames(coef_mat) != "(Intercept)", , drop = FALSE]
  if (nrow(coefs) == 0L) return(NULL)
  
  beta_raw   <- as.numeric(coefs[, 1])
  beta_round <- round(beta_raw, digits)
  
  # Approximate ratio: 100 * coefficient, rounded to 1 decimal
  approx_ratio <- round(100 * abs(beta_round), 1)
  
  data.frame(
    Variable          = rownames(coefs),
    Coefficient       = beta_round,
    Approx_Ratio_100x = approx_ratio,
    row.names = NULL
  )
}

## ================================================================
## 8) Fixed split (seed = 403) + constrained logistic (λ≈0)
##     No AUC is computed or printed.
## ================================================================
fit_constrained_glmnet_nopenalty <- function(
    df_train, df_test, y_name,
    pc_vars,
    lower_lim, upper_lim,
    lambda_small = 1e-10,
    module_label = "module"
) {
  if (length(pc_vars) == 0L) {
    stop(sprintf("No requested PCs found in training data for %s.", module_label))
  }
  
  df_tr <- df_train[, c(y_name, pc_vars), drop = FALSE]
  df_te <- df_test[,  c(y_name, pc_vars), drop = FALSE]
  
  df_tr <- impute_numeric_median(df_tr)
  df_te <- impute_numeric_median(df_te)
  
  y_tr <- df_tr[[y_name]]
  X_tr <- as.matrix(df_tr[, pc_vars, drop = FALSE])
  
  fit <- glmnet(
    x = X_tr, y = y_tr,
    family = "binomial",
    alpha = 0,                   # ridge structure; λ is tiny so essentially unpenalized
    lambda = lambda_small,
    standardize = TRUE,
    lower.limits = lower_lim,
    upper.limits = upper_lim
  )
  
  coefs <- as.matrix(coef(fit))
  tab   <- make_coef_table(coefs, digits = 2)
  
  cat("\n=====================================\n")
  cat(module_label, " — coefficients (rounded to 0.01)\n")
  cat("=====================================\n\n")
  print(tab)
  
  invisible(list(fit = fit, coef = coefs, table = tab))
}

# Build fixed train/test split
set.seed(403)
train_idx <- caret::createDataPartition(merged_data$coliform, p = 0.60, list = FALSE)
train_dat <- merged_data[train_idx, , drop = FALSE]
test_dat  <- merged_data[-train_idx, , drop = FALSE]

## ================================================================
## 9) Module-specific fits for Table 1
##     (Edit PC indices below if Table 1 uses different PCs.)
## ================================================================

# Vulnerability (β ≥ 0)
vul_pc_idx <- c(1, 10)
vul_pcs <- get_pc_vars("vul", vul_pc_idx, train_dat)

fit_vul <- fit_constrained_glmnet_nopenalty(
  df_train     = train_dat,
  df_test      = test_dat,
  y_name       = "coliform",
  pc_vars      = vul_pcs,
  lower_lim    = 0,
  upper_lim    = Inf,
  module_label = "Vulnerability (β ≥ 0; PCs 1, 10)"
)

# Hazard (β ≥ 0)
haz_pc_idx <- c(5, 6)
haz_pcs <- get_pc_vars("haz", haz_pc_idx, train_dat)

fit_haz <- fit_constrained_glmnet_nopenalty(
  df_train     = train_dat,
  df_test      = test_dat,
  y_name       = "coliform",
  pc_vars      = haz_pcs,
  lower_lim    = 0,
  upper_lim    = Inf,
  module_label = "Hazard (β ≥ 0; PCs 5, 6)"
)

# Capacity (β ≤ 0)
cap_pc_idx <- c(2, 4, 8, 10)
cap_pcs <- get_pc_vars("cap", cap_pc_idx, train_dat)

fit_cap <- fit_constrained_glmnet_nopenalty(
  df_train     = train_dat,
  df_test      = test_dat,
  y_name       = "coliform",
  pc_vars      = cap_pcs,
  lower_lim    = -Inf,
  upper_lim    = 0,
  module_label = "Capacity (β ≤ 0; PCs 2, 4, 8, 10)"
)

###############################################################################
# End of script
###############################################################################
