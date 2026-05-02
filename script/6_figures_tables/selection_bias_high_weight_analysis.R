suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(stringr)
  library(tibble)
  library(readxl)
})

output_dir <- "selection_bias_high_weight_outputs"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

escape_latex <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([#$%&_{}])", "\\\\\\1", x, perl = TRUE)
  x <- gsub("~", "\\\\textasciitilde{}", x, fixed = TRUE)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  x
}

escape_latex_preserve_math <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- gsub("\\$-\\$", "CODEXMATHMINUS", x)
  x <- escape_latex(x)
  gsub("CODEXMATHMINUS", "$-$", x, fixed = TRUE)
}

fmt_num <- function(x, digits = 3) {
  out <- ifelse(is.na(x), "", formatC(x, format = "f", digits = digits))
  sub("^-", "$-$", out)
}

fmt_p <- function(x) {
  ifelse(is.na(x), "", ifelse(x < 0.001, "0.001", formatC(x, format = "f", digits = 3)))
}

fmt_summary <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return("NA")
  q <- quantile(x, c(0.25, 0.5, 0.75), names = FALSE)
  paste0(fmt_num(q[2], 2), " [", fmt_num(q[1], 2), ", ", fmt_num(q[3], 2), "]")
}

fmt_mean_sd <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(c(mean = "NA", sd = "NA"))
  c(mean = fmt_num(mean(x), 2), sd = fmt_num(stats::sd(x), 2))
}

format_var_name <- function(x) {
  paste0("\\path{", x, "}")
}

fmt_level_counts <- function(x, max_levels = 3) {
  x <- x[!is.na(x)]
  if (!length(x)) return("NA")
  tab <- sort(table(x), decreasing = TRUE)
  shown <- head(tab, max_levels)
  pct <- round(100 * shown / sum(tab), 1)
  pieces <- paste0(names(shown), " (n=", as.integer(shown), ", ", pct, "%)")
  if (length(tab) > max_levels) pieces <- c(pieces, "...")
  paste(pieces, collapse = "; ")
}

extract_county <- function(x) {
  str_squish(str_to_title(str_extract(x, "[A-Za-z .'-]+ County")))
}

.constrained_pc1 <- function(S, Jpos = integer(), Jneg = integer(),
                             maxit = 2000, tol = 1e-8, nstart = 10) {
  p <- ncol(S)
  best_val <- -Inf
  best_u <- rep(NA_real_, p)

  for (rs in seq_len(nstart)) {
    u <- rnorm(p)
    u <- u / sqrt(sum(u^2))
    prev_val <- -Inf
    for (it in seq_len(maxit)) {
      v <- S %*% u
      if (length(Jpos)) v[Jpos] <- pmax(v[Jpos], 0)
      if (length(Jneg)) v[Jneg] <- pmin(v[Jneg], 0)
      nv <- sqrt(sum(v^2))
      if (nv == 0) {
        u <- rnorm(p)
        u <- u / sqrt(sum(u^2))
        next
      }
      u_new <- v / nv
      cur_val <- drop(t(u_new) %*% S %*% u_new)
      if (abs(cur_val - prev_val) < tol) {
        u <- u_new
        break
      }
      u <- u_new
      prev_val <- cur_val
    }
    val <- drop(t(u) %*% S %*% u)
    if (val > best_val) {
      best_val <- val
      best_u <- u
    }
  }

  list(u = best_u, value = best_val)
}

constrained_pca <- function(X, directionality = NULL, Jpos = integer(), Jneg = integer(),
                            ncomp = 2, center = TRUE, scale. = TRUE,
                            maxit = 2000, tol = 1e-8, nstart = 10, drop_tol = 1e-10) {
  Xs <- scale(X, center = center, scale = scale.)
  S <- crossprod(Xs) / nrow(Xs)
  p <- ncol(S)

  if (!is.null(directionality)) {
    if (length(directionality) != p) stop("Directionality length mismatch.", call. = FALSE)
    Jpos_dir <- which(directionality > 0)
    Jneg_dir <- which(directionality < 0)
  } else {
    Jpos_dir <- integer(0)
    Jneg_dir <- integer(0)
  }
  Jpos <- sort(unique(c(Jpos_dir, Jpos)))
  Jneg <- sort(unique(c(Jneg_dir, Jneg)))

  U <- matrix(NA_real_, p, ncomp)
  vals <- numeric(ncomp)
  Sdef <- S
  for (k in seq_len(ncomp)) {
    pc1 <- .constrained_pc1(Sdef, Jpos = Jpos, Jneg = Jneg, maxit = maxit, tol = tol, nstart = nstart)
    U[, k] <- pc1$u
    vals[k] <- pc1$value
    Sdef <- Sdef - vals[k] * (pc1$u %*% t(pc1$u))
  }

  ord <- order(vals, decreasing = TRUE)
  U <- U[, ord, drop = FALSE]
  vals <- vals[ord]
  keep <- which(vals > drop_tol)
  if (!length(keep)) keep <- 1L
  U <- U[, keep, drop = FALSE]
  vals <- vals[keep]

  list(
    loadings = U,
    sdev = sqrt(pmax(vals, 0)),
    x = Xs %*% U,
    center = attr(Xs, "scaled:center"),
    scale = attr(Xs, "scaled:scale")
  )
}

retain_pc_count <- function(pca_fit, threshold = 0.8) {
  pve <- pca_fit$sdev^2 / sum(pca_fit$sdev^2)
  min(which(cumsum(pve) >= threshold))
}

run_continuous_tests <- function(data, module_name, variables) {
  if (!length(variables)) {
    return(tibble(
      module = character(), variable = character(), type = character(),
      tested_summary = character(), untested_summary = character(), p_value = numeric()
    ))
  }
  bind_rows(lapply(variables, function(var_name) {
    x <- data[[var_name]]
    grp <- data$tested
    keep <- !is.na(x) & !is.na(grp)
    x <- x[keep]
    grp <- grp[keep]
    tested_vals <- x[grp]
    untested_vals <- x[!grp]
    pval <- if (!length(tested_vals) || !length(untested_vals) || length(unique(x)) <= 1) {
      NA_real_
    } else {
      wilcox.test(x ~ grp, exact = FALSE)$p.value
    }
    tibble(
      module = module_name,
      variable = var_name,
      type = "Continuous",
      tested_summary = fmt_summary(tested_vals),
      untested_summary = fmt_summary(untested_vals),
      p_value = pval
    )
  }))
}

run_categorical_tests <- function(data, module_name, variables) {
  if (!length(variables)) {
    return(tibble(
      module = character(), variable = character(), type = character(),
      tested_summary = character(), untested_summary = character(), p_value = numeric()
    ))
  }
  bind_rows(lapply(variables, function(var_name) {
    x <- as.character(data[[var_name]])
    grp <- data$tested
    keep <- !is.na(x) & !is.na(grp)
    x <- x[keep]
    grp <- grp[keep]
    tab <- table(grp, x)
    pval <- if (!length(x) || ncol(tab) <= 1) {
      NA_real_
    } else {
      chi <- suppressWarnings(chisq.test(tab))
      if (any(chi$expected < 5)) {
        suppressWarnings(chisq.test(tab, simulate.p.value = TRUE, B = 10000))$p.value
      } else {
        chi$p.value
      }
    }
    tibble(
      module = module_name,
      variable = var_name,
      type = "Categorical",
      tested_summary = fmt_level_counts(x[grp]),
      untested_summary = fmt_level_counts(x[!grp]),
      p_value = pval
    )
  }))
}

latex_table <- function(df, caption, label, path) {
  cols <- c("variable", "tested_summary", "untested_summary", "p_value", "adj_p_value")
  headers <- c(
    variable = "Variable",
    tested_summary = "Tested cells median [Q1, Q3]",
    untested_summary = "Untested cells median [Q1, Q3]",
    p_value = "P",
    adj_p_value = "BY adj. P"
  )
  tab <- df |>
    mutate(
      p_value = fmt_p(p_value),
      adj_p_value = fmt_p(adj_p_value)
    ) |>
    select(all_of(cols))

  lines <- c(
    "\\small",
    "\\begin{longtable}{p{3.0cm}p{4.3cm}p{4.3cm}cc}",
    paste0("\\caption{", escape_latex(caption), "}\\label{", label, "}\\\\"),
    "\\toprule",
    paste(headers[cols], collapse = " & "),
    " \\\\",
    "\\midrule",
    "\\endfirsthead",
    paste0("\\caption[]{", escape_latex(caption), " (continued)}\\\\"),
    "\\toprule",
    paste(headers[cols], collapse = " & "),
    " \\\\",
    "\\midrule",
    "\\endhead",
    "\\midrule",
    "\\multicolumn{5}{r}{Continued on next page}\\\\",
    "\\midrule",
    "\\endfoot",
    "\\bottomrule",
    "\\endlastfoot"
  )
  for (i in seq_len(nrow(tab))) {
    vals <- vapply(tab[i, cols, drop = FALSE], function(z) escape_latex(z[[1]]), character(1))
    lines <- c(lines, paste(vals, collapse = " & "), " \\\\")
  }
  lines <- c(lines, "\\end{longtable}")
  writeLines(lines, path)
}

build_numeric_descriptives <- function(data, module_name, variables) {
  if (!length(variables)) {
    return(tibble(
      module = character(), variable = character(),
      tested_mean = character(), tested_sd = character(), tested_median_iqr = character(),
      untested_mean = character(), untested_sd = character(), untested_median_iqr = character()
    ))
  }
  bind_rows(lapply(variables, function(var_name) {
    x <- data[[var_name]]
    grp <- data$tested
    keep <- !is.na(x) & !is.na(grp)
    x <- x[keep]
    grp <- grp[keep]
    tested_vals <- x[grp]
    untested_vals <- x[!grp]
    tested_ms <- fmt_mean_sd(tested_vals)
    untested_ms <- fmt_mean_sd(untested_vals)
    tibble(
      module = module_name,
      variable = var_name,
      tested_mean = tested_ms[["mean"]],
      tested_sd = tested_ms[["sd"]],
      tested_median_iqr = fmt_summary(tested_vals),
      untested_mean = untested_ms[["mean"]],
      untested_sd = untested_ms[["sd"]],
      untested_median_iqr = fmt_summary(untested_vals)
    )
  }))
}

build_categorical_descriptives <- function(data, module_name, variables) {
  if (!length(variables)) {
    return(tibble(
      module = character(), variable = character(), level = character(),
      tested_n_pct = character(), untested_n_pct = character()
    ))
  }
  bind_rows(lapply(variables, function(var_name) {
    x <- as.character(data[[var_name]])
    grp <- data$tested
    keep <- !is.na(x) & !is.na(grp)
    x <- x[keep]
    grp <- grp[keep]
    tested_x <- x[grp]
    untested_x <- x[!grp]
    levs <- sort(unique(c(tested_x, untested_x)))
    tested_tab <- table(tested_x)
    untested_tab <- table(untested_x)
    tested_total <- sum(tested_tab)
    untested_total <- sum(untested_tab)
    bind_rows(lapply(levs, function(lev) {
      tn <- if (lev %in% names(tested_tab)) as.integer(tested_tab[[lev]]) else 0L
      un <- if (lev %in% names(untested_tab)) as.integer(untested_tab[[lev]]) else 0L
      tibble(
        module = module_name,
        variable = var_name,
        level = lev,
        tested_n_pct = paste0(tn, " (", fmt_num(100 * tn / tested_total, 1), "%)"),
        untested_n_pct = paste0(un, " (", fmt_num(100 * un / untested_total, 1), "%)")
      )
    }))
  }))
}

latex_numeric_table <- function(df, caption, label, path) {
  df <- df |>
    mutate(variable = vapply(variable, format_var_name, character(1)))
  cols <- c(
    "variable", "tested_mean", "tested_sd", "tested_median_iqr",
    "untested_mean", "untested_sd", "untested_median_iqr"
  )
  lines <- c(
    "\\small",
    "\\begin{longtable}{>{\\raggedright\\arraybackslash}p{4.2cm}cccccc}",
    paste0("\\caption{", escape_latex(caption), "}\\label{", label, "}\\\\"),
    "\\toprule",
    " & \\multicolumn{3}{c}{Tested} & \\multicolumn{3}{c}{Untested} \\\\",
    "\\cmidrule(lr){2-4}\\cmidrule(lr){5-7}",
    "Variable & Mean & SD & Median [Q1, Q3] & Mean & SD & Median [Q1, Q3]",
    " \\\\",
    "\\midrule",
    "\\endfirsthead",
    paste0("\\caption[]{", escape_latex(caption), " (continued)}\\\\"),
    "\\toprule",
    " & \\multicolumn{3}{c}{Tested} & \\multicolumn{3}{c}{Untested} \\\\",
    "\\cmidrule(lr){2-4}\\cmidrule(lr){5-7}",
    "Variable & Mean & SD & Median [Q1, Q3] & Mean & SD & Median [Q1, Q3]",
    " \\\\",
    "\\midrule",
    "\\endhead",
    "\\midrule",
    "\\multicolumn{7}{r}{Continued on next page}\\\\",
    "\\midrule",
    "\\endfoot",
    "\\bottomrule",
    "\\endlastfoot"
  )
  for (i in seq_len(nrow(df))) {
    vals <- vapply(seq_along(cols), function(j) {
      cell <- df[[cols[j]]][[i]]
      if (cols[j] == "variable") cell else escape_latex_preserve_math(cell)
    }, character(1))
    lines <- c(lines, paste(vals, collapse = " & "), " \\\\")
  }
  lines <- c(lines, "\\end{longtable}")
  writeLines(lines, path)
}

latex_categorical_table <- function(df, caption, label, path) {
  df <- df |>
    mutate(variable = vapply(variable, format_var_name, character(1)))
  cols <- c("variable", "level", "tested_n_pct", "untested_n_pct")
  lines <- c(
    "\\small",
    "\\begin{longtable}{>{\\raggedright\\arraybackslash}p{4.0cm}>{\\raggedright\\arraybackslash}p{4.2cm}cc}",
    paste0("\\caption{", escape_latex(caption), "}\\label{", label, "}\\\\"),
    "\\toprule",
    " &  & \\multicolumn{1}{c}{Tested} & \\multicolumn{1}{c}{Untested} \\\\",
    "\\cmidrule(lr){3-3}\\cmidrule(lr){4-4}",
    "Variable & Level & $n$ (\\%) & $n$ (\\%)",
    " \\\\",
    "\\midrule",
    "\\endfirsthead",
    paste0("\\caption[]{", escape_latex(caption), " (continued)}\\\\"),
    "\\toprule",
    " &  & \\multicolumn{1}{c}{Tested} & \\multicolumn{1}{c}{Untested} \\\\",
    "\\cmidrule(lr){3-3}\\cmidrule(lr){4-4}",
    "Variable & Level & $n$ (\\%) & $n$ (\\%)",
    " \\\\",
    "\\midrule",
    "\\endhead",
    "\\midrule",
    "\\multicolumn{4}{r}{Continued on next page}\\\\",
    "\\midrule",
    "\\endfoot",
    "\\bottomrule",
    "\\endlastfoot"
  )
  last_var <- NULL
  for (i in seq_len(nrow(df))) {
    vals <- df[i, cols, drop = FALSE]
    if (!is.null(last_var) && identical(vals$variable[[1]], last_var)) {
      vals$variable[[1]] <- ""
    }
    last_var <- df$variable[[i]]
    esc_vals <- vapply(seq_along(cols), function(j) {
      cell <- vals[[cols[j]]][[1]]
      if (cols[j] == "variable" && nzchar(cell)) cell else escape_latex_preserve_math(cell)
    }, character(1))
    lines <- c(lines, paste(esc_vals, collapse = " & "), " \\\\")
  }
  lines <- c(lines, "\\end{longtable}")
  writeLines(lines, path)
}

# Study domain from tested counties and private-well cells
cap_imp <- st_read("nc_grid_capacity.gpkg", quiet = TRUE) |>
  st_drop_geometry() |>
  select(-any_of("ID")) |>
  mutate(
    across(-any_of(c("grid_id", "block_group_name")), as.numeric),
    county = extract_county(block_group_name)
  )
vul_imp <- st_read("nc_grid_vulnerability_929.gpkg", quiet = TRUE) |>
  st_drop_geometry() |>
  select(-any_of("ID"))
vul_imp_compact <- st_read("nc_grid_vulnerability_imputed.gpkg", quiet = TRUE) |>
  st_drop_geometry() |>
  select(-any_of("ID"))
haz_imp <- st_read("nc_grid_hazard_imputed.gpkg", quiet = TRUE) |>
  st_drop_geometry() |>
  select(-any_of("ID"))
tests <- st_read("well_tests_filtered.gpkg", quiet = TRUE) |>
  st_drop_geometry() |>
  mutate(county = extract_county(block_group_name))

tested_counties <- sort(unique(na.omit(tests$county)))
study_grid <- cap_imp |>
  filter(!is.na(county), county %in% tested_counties) |>
  select(grid_id, county) |>
  inner_join(vul_imp |> filter(Pct_Wells == 100) |> select(grid_id) |> distinct(), by = "grid_id") |>
  distinct(grid_id, county) |>
  mutate(tested = grid_id %in% unique(tests$grid_id))

# Hazard PCA reconstruction
haz_cap <- st_read("nc_grid_capacity.gpkg", quiet = TRUE)
haz_data <- st_read("nc_grid_hazard_imputed.gpkg", quiet = TRUE)
haz_block <- haz_cap |> st_drop_geometry() |> select(grid_id, block_group_name)
haz_data <- haz_data |> left_join(haz_block, by = "grid_id")
for (j in 3:10) haz_data[[j]] <- as.numeric(haz_data[[j]])
haz_data <- na.omit(haz_data)
haz_no_loc <- st_drop_geometry(haz_data)[, -c(1:2, 11)]
haz_scaled <- scale(haz_no_loc)
haz_dir <- rep(1, ncol(haz_scaled))
haz_pca <- constrained_pca(haz_scaled, directionality = haz_dir, ncomp = ncol(haz_scaled), nstart = 30)
rownames(haz_pca$loadings) <- colnames(haz_scaled)
haz_k <- retain_pc_count(haz_pca, 0.8)
haz_loadings <- haz_pca$loadings[, seq_len(haz_k), drop = FALSE]
haz_high_vars <- rownames(haz_loadings)[apply(abs(haz_loadings) > 0.25, 1, any)]

# Capacity PCA reconstruction
cap_capacity <- st_read("nc_grid_capacity_imputed.gpkg", quiet = TRUE)
cap_vul <- st_drop_geometry(st_read("nc_grid_vulnerability_929.gpkg", quiet = TRUE))
cap_pct_wells <- cap_vul |> select(grid_id, Pct_Wells)
cap_data <- cap_pct_wells |>
  left_join(cap_capacity, by = "grid_id") |>
  filter(Pct_Wells == 100)
for (j in 5:27) cap_data[[j]] <- as.numeric(cap_data[[j]])
cap_no_loc <- cap_data[, -(28)]
cap_no_loc <- cap_no_loc[, -(1:4)]
cap_drop <- c("pct_women", "pct_single_parent", "pct_under_17", "pct_women_gave_birth",
              "pct_us_born", "pct_active_commuting", "pct_no_move", "pct_crowded_housing",
              "pct_no_computer", "housing_units_density")
cap_no_loc <- cap_no_loc[, -which(colnames(cap_no_loc) %in% cap_drop)]
cap_scaled <- scale(cap_no_loc)
cap_dir <- c(-1, 1, 1, -1, -1, -1, -1, -1, 1, 1, 1, -1, -1)
cap_pca <- constrained_pca(cap_scaled, directionality = cap_dir, ncomp = ncol(cap_scaled), nstart = 30)
rownames(cap_pca$loadings) <- colnames(cap_scaled)
cap_k <- retain_pc_count(cap_pca, 0.8)
cap_loadings <- cap_pca$loadings[, seq_len(cap_k), drop = FALSE]
cap_high_vars <- rownames(cap_loadings)[apply(abs(cap_loadings) > 0.25, 1, any)]

# Vulnerability PCA reconstruction
vul_data <- st_read("nc_grid_vulnerability_imputed.gpkg", quiet = TRUE)
vul_cap <- st_read("nc_grid_capacity.gpkg", quiet = TRUE)
vul_block <- vul_cap |> st_drop_geometry() |> select(grid_id, block_group_name)
vul_data <- vul_data |> left_join(vul_block, by = "grid_id")
cate.index <- which(colnames(vul_data) %in% c(
  "surfgeo", "KB", "drainage", "aq_rocktype", "ec", "lith", "sar", "weg_int",
  "hydgrp_int", "drainage_class_int", "landuse", "landcover", "str_int", "ec", "sar"
))
for (j in 3:81) {
  if (j %in% cate.index) {
    vul_data[[j]] <- as.factor(vul_data[[j]])
  } else {
    vul_data[[j]] <- as.numeric(vul_data[[j]])
  }
}
vul_data <- na.omit(vul_data)
set.seed(123)
vul_data$y_fake <- rnorm(nrow(vul_data))
vul_no_loc <- st_drop_geometry(vul_data) |>
  select(-matches("A_")) |>
  select(-c(aq_rocktype, weg_int, landuse, drainage_class_int, y_fake, ID, grid_id, block_group_name))
vul_model <- model.matrix(
  ~ surfgeo + KB + lith + hydgrp_int + ec + sar + Pct_Wells + landcover + str_int - 1,
  data = vul_no_loc
)
vul_cate_names <- c("surfgeo", "KB", "lith", "hydgrp_int", "ec", "sar", "Pct_Wells", "landcover", "str_int")
new.cate.index <- which(colnames(vul_no_loc) %in% vul_cate_names)
vul_matrix <- cbind(vul_no_loc[, -new.cate.index], vul_model)
vul_scaled <- scale(vul_matrix)
vul_scaled <- vul_scaled[, which(is.na(apply(vul_scaled, 2, sd)) == FALSE)]
dict <- read_excel("Independent variables for risk prediction model.xlsx")
vul_dir <- rep(NA, ncol(vul_scaled))
for (i in seq_len(ncol(vul_scaled))) {
  nm <- colnames(vul_scaled)[i]
  if (nm %in% dict$`Feature Names (internal)`) {
    rel <- dict$`Relationship with well contamination risk`[match(nm, dict$`Feature Names (internal)`)]
    if (is.na(rel)) {
      vul_dir[i] <- NA
    } else if (rel == "decrease") {
      vul_dir[i] <- -1
    } else if (rel == "increase") {
      vul_dir[i] <- 1
    } else {
      vul_dir[i] <- NA
    }
  }
}
vul_pca <- constrained_pca(vul_scaled, directionality = vul_dir, ncomp = ncol(vul_scaled), nstart = 30)
rownames(vul_pca$loadings) <- colnames(vul_scaled)
vul_k <- retain_pc_count(vul_pca, 0.8)
vul_loadings <- vul_pca$loadings[, seq_len(vul_k), drop = FALSE]
vul_selected_cols <- rownames(vul_loadings)[apply(abs(vul_loadings) > 0.25, 1, any)]
map_parent <- function(nm) {
  candidates <- c("surfgeo", "hydgrp_int", "landcover", "str_int", "Pct_Wells", "lith", "sar", "ec", "KB")
  hit <- candidates[startsWith(nm, candidates)]
  if (length(hit)) hit[which.max(nchar(hit))] else nm
}
vul_high_vars <- sort(unique(vapply(vul_selected_cols, map_parent, character(1))))
vul_high_vars <- setdiff(vul_high_vars, "Pct_Wells")

pc_counts <- tibble(
  module = c("Hazard", "Capacity", "Vulnerability"),
  retained_pcs_80pct = c(haz_k, cap_k, vul_k)
)
write.csv(pc_counts, file.path(output_dir, "retained_pc_counts.csv"), row.names = FALSE)

selected_vars <- tibble(
  module = c(rep("Hazard", length(haz_high_vars)),
             rep("Capacity", length(cap_high_vars)),
             rep("Vulnerability", length(vul_high_vars))),
  variable = c(haz_high_vars, cap_high_vars, vul_high_vars)
)
write.csv(selected_vars, file.path(output_dir, "high_weight_variables.csv"), row.names = FALSE)

# Two-sample tests on filtered variable sets
haz_dat <- study_grid |> left_join(haz_imp, by = "grid_id")
cap_dat <- study_grid |> left_join(cap_imp, by = "grid_id")
vul_dat <- study_grid |> left_join(vul_imp, by = "grid_id")
vul_dat_cat <- study_grid |> left_join(vul_imp_compact, by = "grid_id")

vul_categorical <- intersect(vul_high_vars, c("surfgeo", "KB", "lith", "aq_rocktype", "ec", "sar", "str_int", "weg_int", "hydgrp_int", "drainage_class_int", "landcover", "landuse"))
vul_continuous <- setdiff(vul_high_vars, vul_categorical)
haz_categorical <- intersect(haz_high_vars, "fema_floodplain")
haz_continuous <- setdiff(haz_high_vars, haz_categorical)

results <- bind_rows(
  run_continuous_tests(cap_dat, "Capacity", cap_high_vars),
  run_continuous_tests(haz_dat, "Hazard", haz_continuous),
  run_categorical_tests(haz_dat, "Hazard", haz_categorical),
  run_continuous_tests(vul_dat, "Vulnerability", vul_continuous),
  run_categorical_tests(vul_dat_cat, "Vulnerability", vul_categorical)
) |>
  mutate(adj_p_value = p.adjust(p_value, method = "BY")) |>
  arrange(module, adj_p_value, p_value, variable)

summary_tab <- results |>
  group_by(module) |>
  summarise(
    variables_tested = n(),
    significant_by_005 = sum(adj_p_value < 0.05, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(results, file.path(output_dir, "high_weight_variable_tests.csv"), row.names = FALSE)
write.csv(summary_tab, file.path(output_dir, "high_weight_variable_summary.csv"), row.names = FALSE)

cap_numeric_desc <- build_numeric_descriptives(cap_dat, "Capacity", cap_high_vars)
haz_numeric_desc <- build_numeric_descriptives(haz_dat, "Hazard", haz_continuous)
haz_categorical_desc <- build_categorical_descriptives(haz_dat, "Hazard", haz_categorical)
vul_numeric_desc <- build_numeric_descriptives(vul_dat, "Vulnerability", vul_continuous)
vul_categorical_desc <- build_categorical_descriptives(vul_dat_cat, "Vulnerability", vul_categorical)

write.csv(cap_numeric_desc, file.path(output_dir, "selection_bias_capacity_numeric.csv"), row.names = FALSE)
write.csv(haz_numeric_desc, file.path(output_dir, "selection_bias_hazard_numeric.csv"), row.names = FALSE)
write.csv(haz_categorical_desc, file.path(output_dir, "selection_bias_hazard_categorical.csv"), row.names = FALSE)
write.csv(vul_numeric_desc, file.path(output_dir, "selection_bias_vulnerability_numeric.csv"), row.names = FALSE)
write.csv(vul_categorical_desc, file.path(output_dir, "selection_bias_vulnerability_categorical.csv"), row.names = FALSE)

latex_numeric_table(cap_numeric_desc,
                    "Selection-bias screening for retained high-weight social-capacity variables.",
                    "tab:sel_bias_capacity_num",
                    file.path(output_dir, "selection_bias_capacity_numeric.tex"))
latex_numeric_table(haz_numeric_desc,
                    "Selection-bias screening for retained high-weight hazard variables.",
                    "tab:sel_bias_hazard_num",
                    file.path(output_dir, "selection_bias_hazard_numeric.tex"))
latex_categorical_table(haz_categorical_desc,
                        "Selection-bias screening for retained high-weight categorical hazard variables.",
                        "tab:sel_bias_hazard_cat",
                        file.path(output_dir, "selection_bias_hazard_categorical.tex"))
latex_numeric_table(vul_numeric_desc,
                    "Selection-bias screening for retained high-weight physical-vulnerability variables.",
                    "tab:sel_bias_vulnerability_num",
                    file.path(output_dir, "selection_bias_vulnerability_numeric.tex"))
latex_categorical_table(vul_categorical_desc,
                        "Selection-bias screening for retained high-weight categorical physical-vulnerability variables.",
                        "tab:sel_bias_vulnerability_cat",
                        file.path(output_dir, "selection_bias_vulnerability_categorical.tex"))

summary_lines <- c(
  "\\begin{tabular}{lrr}",
  "\\toprule",
  "Module & Retained PCs & High-weight variables tested \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(pc_counts))) {
  mod <- pc_counts$module[i]
  nvars <- summary_tab$variables_tested[match(mod, summary_tab$module)]
  summary_lines <- c(summary_lines,
                     paste(mod, pc_counts$retained_pcs_80pct[i], nvars, sep = " & "),
                     " \\\\")
}
summary_lines <- c(summary_lines, "\\bottomrule", "\\end{tabular}")
writeLines(summary_lines, file.path(output_dir, "high_weight_summary.tex"))

report_lines <- c(
  "\\documentclass[11pt]{article}",
  "\\usepackage[margin=1in]{geometry}",
  "\\usepackage{booktabs}",
  "\\usepackage{longtable}",
  "\\usepackage{array}",
  "\\usepackage{pdflscape}",
  "\\usepackage{caption}",
  "\\usepackage{mathpazo}",
  "\\begin{document}",
  "\\title{Selection-Bias Tests for High-Weight PCA Variables}",
  "\\author{}",
  "\\date{}",
  "\\maketitle",
  "\\section*{Design}",
  "Within each module, PCs were retained until they jointly explained at least 80\\% of the total variance. High-weight variables were defined as those with absolute PCA loadings exceeding 0.25 on at least one retained PC. The screening tables report descriptive summaries only: mean, standard deviation, and median [Q1, Q3] for numerical variables, and level-specific counts and percentages for categorical variables.",
  "\\section*{Summary}",
  "\\input{high_weight_summary.tex}",
  "\\begin{landscape}",
  "\\section*{Detailed Tables}",
  "\\subsection*{Social Capacity}",
  "\\input{selection_bias_capacity_numeric.tex}",
  "\\subsection*{Hazard}",
  "\\input{selection_bias_hazard_numeric.tex}",
  "\\input{selection_bias_hazard_categorical.tex}",
  "\\subsection*{Physical Vulnerability}",
  "\\input{selection_bias_vulnerability_numeric.tex}",
  "\\input{selection_bias_vulnerability_categorical.tex}",
  "\\end{landscape}",
  "\\end{document}"
)
writeLines(report_lines, file.path(output_dir, "selection_bias_high_weight_report.tex"))

message("High-weight PCA variable analysis complete.")
