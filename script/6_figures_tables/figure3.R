###############################################################################
# Figure 3 — ROC comparison for three Random Forest (RF) models
# Manuscript: Hybrid supervised–unsupervised modeling for post-hurricane private
# well contamination risk score using empirical validation and community
# ground-truthing
#
# Models compared:
#   1) Hybrid (PC-only): time + all PCs across the three modules
#   2) Kitchen-sink (Non-PC): all original variables from modules (minus drops)
#   3) Unsupervised: time + first PC from each module (haz_PC1, vul_PC1, cap_PC1)
#
# Procedure:
#   - Fixed stratified train/test split (split_seed)
#   - For each feature set: align train/test predictors, impute, harmonize levels
#   - Stratified k-fold CV tunes (mtry, nodesize) by mean AUC
#   - Final RF fit with tuned hyperparameters (ntree fixed)
#   - Probability cutoff chosen to maximize TRAIN F1
#   - Report Train/Test AUC & F1; plot TEST ROC curves (manual FPR vs TPR)
#
# Notes:
#   - Uses dplyr::select explicitly to avoid select() conflicts.
#   - RF randomness (CV folds + RF training) controlled by rf_seed_use.
###############################################################################

# ---------------------------
# 0) Packages
# ---------------------------
suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(caret)
  library(randomForest)
  library(pROC)
  library(tidyselect)
})

# ---------------------------
# 1) Reproducibility controls
# ---------------------------
split_seed  <- 403
rf_seed_use <- 166
ntree_use   <- 2000
k_folds     <- 5

# ---------------------------
# 2) Read data
# ---------------------------
data <- sf::st_read("validation.gpkg", quiet = TRUE)
data$time <- as.numeric(data$Date_Collected - as.Date("2024-09-25"))

vul_dat <- sf::st_read("index_vulnerability_detailed.gpkg", quiet = TRUE)
haz_dat <- sf::st_read("index_hazard_detailed.gpkg", quiet = TRUE)
cap_dat <- sf::st_read("index_capacity_detailed.gpkg", quiet = TRUE)

# ---------------------------
# 3) Outcome + merge
# ---------------------------
data$coliform <- ifelse(data$Coliform_Result_Binary == "Present", 1L, 0L)

vul_tab <- sf::st_drop_geometry(vul_dat)
haz_tab <- sf::st_drop_geometry(haz_dat)
cap_tab <- sf::st_drop_geometry(cap_dat)

drop_cols <- c(
  "System_Name", "Street_Address", "City", "State", "Zip",
  "Street_Address_Concat", "EIN", "StarLiMS_ID",
  "Date_Collected", "Sampling_Point", "Comment",
  "Accuracy", "Accuracy_Type", "grid_id",
  "index_vulnerability", "index_hazard", "index_capacity",
  "risk", "Total_Coliform_Result", "Ecoli_Result",
  "Ecoli_Result_Binary", "Coliform_Result_Binary", "ecoli", "y_fake"
)

merged_data <- data %>%
  dplyr::left_join(vul_tab, by = "grid_id") %>%
  dplyr::left_join(haz_tab, by = "grid_id") %>%
  dplyr::left_join(cap_tab, by = "grid_id") %>%
  sf::st_drop_geometry() %>%
  dplyr::select(
    -tidyselect::matches("\\.x$"),
    -tidyselect::matches("\\.y$"),
    -tidyselect::matches("\\.x\\.x$"),
    -tidyselect::matches("\\.y\\.y$")
  ) %>%
  dplyr::select(
    -tidyselect::starts_with("A_"),
    -tidyselect::any_of(drop_cols)
  )

# ---------------------------
# 4) Remove all-NA / constant columns; coerce types
# ---------------------------
is_all_na <- vapply(merged_data, function(x) all(is.na(x)), logical(1))
is_const  <- vapply(merged_data, function(x) length(unique(na.omit(x))) <= 1, logical(1))
merged_data <- merged_data[, !(is_all_na | is_const), drop = FALSE]

merged_data[] <- lapply(merged_data, function(x) if (is.character(x)) factor(x) else x)

stopifnot("coliform" %in% names(merged_data))
merged_data$coliform <- as.integer(merged_data$coliform == 1L)
stopifnot(length(unique(merged_data$coliform)) == 2L)

# ---------------------------
# 5) Fixed stratified train/test split
# ---------------------------
set.seed(split_seed)
train_idx <- caret::createDataPartition(merged_data$coliform, p = 0.6, list = FALSE)
train_dat <- merged_data[train_idx, , drop = FALSE]
test_dat  <- merged_data[-train_idx, , drop = FALSE]

cat(sprintf("Training size: %d | Testing size: %d\n", nrow(train_dat), nrow(test_dat)))
cat(sprintf("Class balance (train): %.2f%% positives\n", 100 * mean(train_dat$coliform)))
cat(sprintf("Class balance (test):  %.2f%% positives\n", 100 * mean(test_dat$coliform)))

###############################################################################
# 6) Utilities: preprocessing, CV tuning, final fit
###############################################################################

# ---- Basic helpers ----
coerce_numeric_like <- function(x) {
  if (is.factor(x)) {
    suppressWarnings({ y <- as.numeric(as.character(x)) })
    if (!all(is.na(y))) return(y)
  }
  x
}

normalize_empty_to_na <- function(x) {
  if (!is.character(x)) return(x)
  x_trim <- trimws(x)
  x_trim[x_trim %in% c("", "NA", "N/A", "na", "n/a", "<NA>")] <- NA_character_
  x_trim
}

fct_mode <- function(f) {
  tab <- table(f)
  names(tab)[which.max(tab)]
}

# ---- Align TRAIN/TEST on predictors; impute; harmonize levels ----
impute_and_align <- function(train, test, vars, outcome = "coliform", other = "__OTHER__") {
  use_vars <- intersect(vars, intersect(names(train), names(test)))
  if (length(use_vars) == 0L) stop("No overlapping predictors between train/test for this feature set.")
  
  tr <- train[, c(outcome, use_vars), drop = FALSE]
  te <- test[,  c(outcome, use_vars), drop = FALSE]
  
  tr[] <- lapply(tr, coerce_numeric_like)
  te[] <- lapply(te, coerce_numeric_like)
  
  tr[] <- lapply(tr, function(x) if (is.character(x)) normalize_empty_to_na(x) else x)
  te[] <- lapply(te, function(x) if (is.character(x)) normalize_empty_to_na(x) else x)
  
  common <- setdiff(intersect(names(tr), names(te)), outcome)
  
  for (v in common) {
    if (is.numeric(tr[[v]]) || is.integer(tr[[v]])) {
      med <- suppressWarnings(median(tr[[v]], na.rm = TRUE))
      if (!is.finite(med)) med <- 0
      tr[[v]][is.na(tr[[v]])] <- med
      te[[v]][is.na(te[[v]])] <- med
      
      if (!(is.numeric(te[[v]]) || is.integer(te[[v]]))) {
        suppressWarnings(te[[v]] <- as.numeric(as.character(te[[v]])))
        te[[v]][is.na(te[[v]])] <- med
      }
    } else {
      tr_chr <- as.character(tr[[v]])
      te_chr <- as.character(te[[v]])
      
      tr_mode <- fct_mode(factor(tr_chr))
      tr_chr[is.na(tr_chr)] <- tr_mode
      te_chr[is.na(te_chr)] <- tr_mode
      
      L_train <- sort(unique(tr_chr))
      te_chr[!(te_chr %in% L_train)] <- other
      L_full <- unique(c(L_train, other))
      
      tr[[v]] <- factor(tr_chr, levels = L_full)
      te[[v]] <- factor(te_chr, levels = L_full)
    }
  }
  
  # Drop zero-variance in TRAIN (mirror in TEST)
  nzv <- vapply(tr[, common, drop = FALSE], function(x) {
    if (is.numeric(x) || is.integer(x)) sd(x, na.rm = TRUE) > 0
    else if (is.factor(x)) length(unique(x)) > 1
    else TRUE
  }, logical(1))
  
  keep <- names(which(nzv))
  tr <- tr[, c(outcome, keep), drop = FALSE]
  te <- te[, c(outcome, keep), drop = FALSE]
  
  list(train = tr, test = te)
}

# ---- Outcome & metrics ----
make_posneg <- function(y) {
  y_chr <- as.character(y)
  pos_vals <- c("1","TRUE","T","Yes","Y","pos","Pos","positive","case")
  factor(ifelse(y_chr %in% pos_vals, "pos", "neg"), levels = c("pos","neg"))
}

make_negpos_for_pROC <- function(y_posneg) {
  factor(ifelse(y_posneg == "pos", "pos", "neg"), levels = c("neg","pos"))
}

roc_auc_pair <- function(y_negpos, prob_pos) {
  r <- pROC::roc(response = y_negpos, predictor = prob_pos,
                 levels = c("neg","pos"), quiet = TRUE)
  list(roc = r, auc = as.numeric(pROC::auc(r)))
}

f1_score <- function(y_true, y_pred) {
  tp <- sum(y_true == "pos" & y_pred == "pos")
  fp <- sum(y_true == "neg" & y_pred == "pos")
  fn <- sum(y_true == "pos" & y_pred == "neg")
  
  precision <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
  recall    <- ifelse(tp + fn == 0, 0, tp / (tp + fn))
  if (precision + recall == 0) return(0)
  2 * precision * recall / (precision + recall)
}

optimal_f1_cutoff <- function(y_true_posneg, prob_pos) {
  y_true_posneg <- factor(y_true_posneg, levels = c("pos","neg"))
  cuts <- seq(0.01, 0.99, by = 0.01)
  f1_vals <- numeric(length(cuts))
  for (i in seq_along(cuts)) {
    pred <- ifelse(prob_pos >= cuts[i], "pos", "neg")
    f1_vals[i] <- f1_score(y_true_posneg, pred)
  }
  cuts[which.max(f1_vals)]
}

# ---- Stratified folds ----
stratified_folds <- function(y, k = 5, seed = 123) {
  set.seed(seed)
  y <- factor(y)
  folds <- vector("list", k)
  for (lev in levels(y)) {
    idx <- which(y == lev)
    idx <- sample(idx, length(idx))
    parts <- split(idx, rep(1:k, length.out = length(idx)))
    for (i in seq_len(k)) folds[[i]] <- c(folds[[i]], parts[[i]])
  }
  lapply(folds, sort)
}

# ---- CV tuning (mtry, nodesize) by mean AUC ----
cv_tune_rf <- function(train_df, mtry_grid, nodesize_grid,
                       k = 5, ntree = 2000,
                       quiet = FALSE, rf_seed = 123) {
  
  y <- make_posneg(train_df$coliform)
  folds <- stratified_folds(y, k = k, seed = rf_seed)
  
  p <- ncol(train_df) - 1L
  mtry_grid     <- sort(unique(pmax(1L, pmin(p, as.integer(mtry_grid)))))
  nodesize_grid <- sort(unique(as.integer(nodesize_grid)))
  
  res_tbl <- data.frame(mtry = integer(), nodesize = integer(), mean_auc = numeric())
  
  for (mtry_g in mtry_grid) {
    for (nodesize_g in nodesize_grid) {
      
      aucs <- numeric(length(folds))
      
      for (i in seq_along(folds)) {
        val_idx <- folds[[i]]
        tr_idx  <- setdiff(seq_len(nrow(train_df)), val_idx)
        
        tr <- train_df[tr_idx, , drop = FALSE]
        va <- train_df[val_idx, , drop = FALSE]
        
        tr$coliform <- make_posneg(tr$coliform)
        va$coliform <- make_posneg(va$coliform)
        
        # Deterministic RF seeds within grid/fold
        set.seed(rf_seed + mtry_g + nodesize_g + i)
        
        rf <- randomForest(
          coliform ~ ., data = tr,
          ntree    = ntree,
          mtry     = mtry_g,
          nodesize = nodesize_g
        )
        
        prob_va <- as.numeric(predict(rf, va, type = "prob")[, "pos"])
        aucs[i] <- roc_auc_pair(make_negpos_for_pROC(va$coliform), prob_va)$auc
      }
      
      mean_auc <- mean(aucs, na.rm = TRUE)
      
      if (!quiet) {
        cat(sprintf("[CV] seed=%d | mtry=%d nodesize=%d -> mean AUC=%.3f\n",
                    rf_seed, mtry_g, nodesize_g, mean_auc))
      }
      
      res_tbl <- rbind(res_tbl,
                       data.frame(mtry = mtry_g, nodesize = nodesize_g, mean_auc = mean_auc))
    }
  }
  
  res_tbl[which.max(res_tbl$mean_auc), , drop = FALSE]
}

# ---- Final tuned fit; cutoff via TRAIN F1 ----
fit_rf_tuned <- function(train_df, test_df,
                         best_mtry, best_nodesize,
                         ntree = 2000, quiet = FALSE,
                         rf_seed = 123) {
  
  train_df$coliform <- make_posneg(train_df$coliform)
  test_df$coliform  <- make_posneg(test_df$coliform)
  
  set.seed(rf_seed)
  rf_mod <- randomForest(
    coliform ~ ., data = train_df,
    ntree    = ntree,
    mtry     = best_mtry,
    nodesize = best_nodesize
  )
  
  prob_tr <- as.numeric(predict(rf_mod, train_df, type = "prob")[, "pos"])
  prob_te <- as.numeric(predict(rf_mod, test_df,  type = "prob")[, "pos"])
  
  best_cut <- optimal_f1_cutoff(train_df$coliform, prob_tr)
  
  roc_tr <- roc_auc_pair(make_negpos_for_pROC(train_df$coliform), prob_tr)
  roc_te <- roc_auc_pair(make_negpos_for_pROC(test_df$coliform),  prob_te)
  
  pred_tr <- ifelse(prob_tr >= best_cut, "pos", "neg")
  pred_te <- ifelse(prob_te >= best_cut, "pos", "neg")
  
  f1_tr <- f1_score(train_df$coliform, pred_tr)
  f1_te <- f1_score(test_df$coliform,  pred_te)
  
  if (!quiet) {
    cat(sprintf(
      "[FINAL RF] seed=%d | mtry=%d nodesize=%d cutoff=%.2f | Train AUC=%.3f, F1=%.3f | Test AUC=%.3f, F1=%.3f\n",
      rf_seed, best_mtry, best_nodesize, best_cut,
      roc_tr$auc, f1_tr, roc_te$auc, f1_te
    ))
  }
  
  list(
    model      = rf_mod,
    auc_train  = roc_tr$auc,
    auc_test   = roc_te$auc,
    f1_train   = f1_tr,
    f1_test    = f1_te,
    prob_train = prob_tr,
    prob_test  = prob_te,
    cutoff     = best_cut,
    roc_train  = roc_tr$roc,
    roc_test   = roc_te$roc
  )
}

# ---- Run one method block ----
run_block <- function(vars, tag, ntree = 2000, quiet = FALSE, rf_seed = 123) {
  
  pair <- impute_and_align(train_dat, test_dat, vars)
  train_red <- pair$train
  test_red  <- pair$test
  
  p <- ncol(train_red) - 1L
  mtry_grid <- unique(round(c(sqrt(p)/2, sqrt(p), 2*sqrt(p), p/3)))
  mtry_grid <- pmax(1L, pmin(p, mtry_grid))
  nodesize_grid <- c(1, 3, 5, 10, 20)
  
  if (!quiet) cat(sprintf("\n===== %s — CV tuning (rf_seed=%d) =====\n", tag, rf_seed))
  best <- cv_tune_rf(train_red, mtry_grid, nodesize_grid,
                     k = k_folds, ntree = ntree,
                     quiet = quiet, rf_seed = rf_seed)
  
  if (!quiet) {
    cat(sprintf("Best params for %s: mtry=%d, nodesize=%d (mean CV AUC=%.3f)\n",
                tag, best$mtry, best$nodesize, best$mean_auc))
    cat(sprintf("===== %s — Fit final tuned RF (rf_seed=%d) =====\n", tag, rf_seed))
  }
  
  fit <- fit_rf_tuned(train_red, test_red,
                      best_mtry = best$mtry,
                      best_nodesize = best$nodesize,
                      ntree = ntree,
                      quiet = quiet,
                      rf_seed = rf_seed)
  
  list(best = best, fit = fit)
}

###############################################################################
# 7) Feature sets
###############################################################################
all_vars   <- setdiff(names(train_dat), "coliform")
pc_vars    <- grep("_PC", all_vars, value = TRUE)
nonpc_vars <- setdiff(all_vars, pc_vars)

drop_cap_vars <- c(
  "pct_women", "pct_single_parent", "pct_under_17", "pct_women_gave_birth",
  "pct_us_born", "pct_active_commuting", "pct_no_move", "pct_crowded_housing",
  "pct_no_computer", "housing_units_density"
)
nonpc_vars <- setdiff(nonpc_vars, drop_cap_vars)

pc_vars    <- c("time", pc_vars)
unsup_vars <- c("time", "haz_PC1", "vul_PC1", "cap_PC1")

###############################################################################
# 8) Fit three methods (same rf_seed)
###############################################################################
res_hybrid <- run_block(pc_vars,
                        tag = "Hybrid (PC-only)",
                        ntree = ntree_use, quiet = TRUE,
                        rf_seed = rf_seed_use)

res_kitchen <- run_block(nonpc_vars,
                         tag = "Kitchen-sink (Non-PC)",
                         ntree = ntree_use, quiet = TRUE,
                         rf_seed = rf_seed_use)

res_unsup <- run_block(unsup_vars,
                       tag = "Unsupervised (PC1 per module)",
                       ntree = ntree_use, quiet = TRUE,
                       rf_seed = rf_seed_use)

###############################################################################
# 9) Summary table
###############################################################################
comp_tbl <- data.frame(
  method = c("Hybrid (PC-only)", "Kitchen-sink (Non-PC)", "Unsupervised (PC1 only)"),
  auc_train = c(res_hybrid$fit$auc_train, res_kitchen$fit$auc_train, res_unsup$fit$auc_train),
  auc_test  = c(res_hybrid$fit$auc_test,  res_kitchen$fit$auc_test,  res_unsup$fit$auc_test),
  f1_train  = c(res_hybrid$fit$f1_train,  res_kitchen$fit$f1_train,  res_unsup$fit$f1_train),
  f1_test   = c(res_hybrid$fit$f1_test,   res_kitchen$fit$f1_test,   res_unsup$fit$f1_test),
  cutoff    = c(res_hybrid$fit$cutoff,    res_kitchen$fit$cutoff,    res_unsup$fit$cutoff),
  mtry      = c(res_hybrid$best$mtry,     res_kitchen$best$mtry,     res_unsup$best$mtry),
  nodesize  = c(res_hybrid$best$nodesize, res_kitchen$best$nodesize, res_unsup$best$nodesize),
  mean_cv_auc = c(res_hybrid$best$mean_auc, res_kitchen$best$mean_auc, res_unsup$best$mean_auc)
)

cat(sprintf("\n================ METHOD COMPARISON (rf_seed=%d) ================\n", rf_seed_use))
print(comp_tbl)

###############################################################################
# 10) ROC plot (TEST) — manual FPR vs TPR overlay
###############################################################################
roc_xy <- function(roc_obj) {
  xy <- pROC::coords(
    roc_obj,
    x = "all",
    ret = c("specificity", "sensitivity"),
    transpose = FALSE
  )
  fpr <- 1 - xy[, "specificity"]
  tpr <- xy[, "sensitivity"]
  ord <- order(fpr, tpr)
  data.frame(fpr = fpr[ord], tpr = tpr[ord])
}

xy_h <- roc_xy(res_hybrid$fit$roc_test)
xy_k <- roc_xy(res_kitchen$fit$roc_test)
xy_u <- roc_xy(res_unsup$fit$roc_test)

op <- par(no.readonly = TRUE)
par(xaxs = "i", yaxs = "i")

plot(
  xy_h$fpr, xy_h$tpr,
  type = "l", lwd = 2, col = "magenta",
  xlim = c(0, 1), ylim = c(0, 1),
  main = "Receiver Operating Characteristic Curve",
  xlab = "False Positive Rate (1 - Specificity)",
  ylab = "True Positive Rate (Sensitivity)"
)
lines(xy_k$fpr, xy_k$tpr, lwd = 2, col = "deepskyblue")
lines(xy_u$fpr, xy_u$tpr, lwd = 2, col = "indianred1")

abline(a = 0, b = 1, lty = 2, col = "gray50")

legend(
  "bottomright",
  legend = c(
    sprintf("Hybrid (AUC = %.3f)",       res_hybrid$fit$auc_test),
    sprintf("Kitchen-sink (AUC = %.3f)", res_kitchen$fit$auc_test),
    sprintf("Unsupervised (AUC = %.3f)", res_unsup$fit$auc_test)
  ),
  col = c("magenta", "deepskyblue", "indianred1"),
  lwd = 2,
  bty = "n"
)

par(op)
