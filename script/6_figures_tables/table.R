############################################# load packages
library("MASS")
library("dplyr")
library("factoextra")
library("exactextractr")
library("tigris")
library("ggplot2")
library("plotly")
library("sf")
library("tibble")
library("GGally")
library("cluster")
library("mclust")
library("stringr")
library("tigris")
library("tidyverse")
library("ggnewscale")
library('xtable')
library("dplyr")
library("officer")
library("flextable")
library("stringr")

############################################# set working directory
setwd('~/Desktop/Research/Contamination risk/')

############################################# read data
index_capacity <- st_read('index_capacity.gpkg')
index_vulnerability <- st_read('index_vulnerability.gpkg')
index_hazard <- st_read('index_hazard.gpkg')

############################################# data manipulation
colnames(index_capacity)[4] <- 'index_capacity'
colnames(index_hazard)[4] <- 'index_hazard'
colnames(index_vulnerability)[4] <- 'index_vulnerability'

############################################# merging
index <- index_vulnerability %>% st_drop_geometry() %>% 
  left_join(index_hazard, by = "grid_id") %>%
  left_join(index_capacity, by = "grid_id")
index <- index[, c(1, 2, 3, 4, 7, 11, 8)]
colnames(index)[c(1, 3, 7)] <- c('ID', 'block_group_name', 'geom')
index <- st_as_sf(index, sf_column_name = "geom")

#############################################  risk score computation
index$risk <- NA_real_
ok <- !is.na(index$index_capacity)
index$risk[ok] <- index$index_vulnerability[ok] * index$index_hazard[ok] / exp(2 * index$index_capacity[ok])
index$risk[ok] <- (index$risk[ok] - min(index$risk[ok])) / (max(index$risk[ok]) - min(index$risk[ok]))
st_write(index, 'nc_grid_index.gpkg', layer = "my_layer", driver = "GPKG", append = FALSE)

#############################################  target counties
target_counties <- c("Alexander County","Alleghany County","Ashe County","Avery County",
                     "Buncombe County","Burke County","Caldwell County","Catawba County",
                     "Clay County","Cleveland County","Gaston County","Haywood County",
                     "Henderson County","Jackson County","Lincoln County","Macon County",
                     "Madison County","Mcdowell County","Mitchell County","Polk County",
                     "Rutherford County","Transylvania County","Watauga County","Wilkes County",
                     "Yancey County")


############################################# NC counties
NC_counties <- tigris::counties(state = "NC", year = 2024)
western_counties <- NC_counties %>% filter(NAMELSAD %in% target_counties)

############################################# subset of the data that lie in those target counties
index <- index %>% mutate(county_raw = str_extract(block_group_name, "[A-Za-z .'-]+ County"),
                          county_norm = str_squish(str_to_title(county_raw)))
index_subset <- index %>% filter(!is.na(county_norm) & county_norm %in% target_counties)



###### summary statistics
iqr <- function(x) paste0(round(quantile(na.omit(x), .25), 2), '-', 
                          round(quantile(na.omit(x), .75), 2))

summ <- function(x, names) {
  p <- ncol(x)
  n <- nrow(x)
  tab <- data.frame()
  for (j in 1:p) {
    x.j <- x[, j]
    info <- c(round(mean(x.j, na.rm = TRUE), 2),
              round(sd(x.j, na.rm = TRUE), 2),
              round(median(x.j, na.rm = TRUE), 2),
              iqr(x.j))
    tab <- rbind(tab, info)
  }
  tab <- cbind(names, tab)
  colnames(tab) <- c('Name', 'Mean', 'SD', 'Median', 'IQR')
  print(tab)
}


################################################################## Table S2

############################ summary statistics for vulnerability
data_vulnerability <- st_read('nc_grid_vulnerability_imputed.gpkg')
data_vulnerability <- data_vulnerability %>% st_drop_geometry()
index_subset <- index_subset %>% left_join(data_vulnerability, by = "grid_id")
vulnerability_subset <- index_subset %>% st_drop_geometry()
vulnerability_subset <- vulnerability_subset[which(vulnerability_subset$Pct_Wells == 100), 11:73]
## continuous variable
num_var <- which(sapply(vulnerability_subset, class) == 'numeric')
summary <- summ(vulnerability_subset[, num_var], 
                names = colnames(vulnerability_subset)[num_var])
xtable(summary)

## categorical variable
cate_var <- which(sapply(vulnerability_subset, class) != 'numeric')
cate_var <- c(cate_var, 42)
for (j in cate_var) {
  print(table(vulnerability_subset[, j]))
}

#################### hazard module
data_hazard <- st_read('nc_grid_hazard_imputed.gpkg')
index_subset <- index_subset %>% st_drop_geometry() %>% left_join(data_hazard, by = "grid_id")
hazard_subset <- index_subset %>% st_drop_geometry()
colnames(index_subset)[1] <- 'ID'
hazard_subset <- hazard_subset[hazard_subset$Pct_Wells == 100, 75:82]

## continuous variable
num_var <- which(sapply(hazard_subset, class) == 'numeric')
summary <- summ(hazard_subset[, num_var], 
                names = colnames(hazard_subset)[num_var])

xtable(summary)

## categorical variable
cate_var <- which(sapply(hazard_subset, class) != 'numeric')
print(table(hazard_subset[, cate_var]))




############################ social capacity index
data_capacity <- st_read('nc_grid_capacity_imputed.gpkg')
data_capacity <- data_capacity %>% st_drop_geometry()
index_subset <- index_subset %>% left_join(data_capacity, by = "grid_id")
capacity_subset <- index_subset %>% st_drop_geometry()
capacity_subset <- capacity_subset[capacity_subset$Pct_Wells == 100, 86:108]

## continuous variable
num_var <- which(sapply(capacity_subset, class) == 'numeric')
summary <- summ(capacity_subset[, num_var], 
                names = colnames(capacity_subset)[num_var])

xtable(summary)

################################################################## Table 1
risk_summary_by_county <- index_subset %>%
  group_by(county_norm) %>%
  summarise(
    Mean   = mean(risk, na.rm = TRUE),
    SD     = sd(risk, na.rm = TRUE),
    `25%`  = quantile(risk, 0.25, na.rm = TRUE),
    Median = median(risk, na.rm = TRUE),
    `75%`  = quantile(risk, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(-county_norm, ~ round(.x, 2))) %>%
  arrange(county_norm)

ft <- flextable(risk_summary_by_county)
ft <- autofit(ft)
ft <- set_caption(ft, caption = "Table S3. Descriptive statistics of risk index by county.")

# Export to Word
doc <- read_docx() %>%
  body_add_flextable(ft) %>%
  body_add_par("", style = "Normal")

print(doc, target = "risk_summary_by_county.docx")


summarise_index <- function(data, county_col, value_col) {
  county_sym <- rlang::ensym(county_col)
  value_sym  <- rlang::ensym(value_col)
  
  data %>%
    group_by(!!county_sym) %>%
    summarise(
      Mean   = mean(!!value_sym, na.rm = TRUE),
      SD     = sd(!!value_sym, na.rm = TRUE),
      `25%`  = quantile(!!value_sym, 0.25, na.rm = TRUE),
      Median = median(!!value_sym, na.rm = TRUE),
      `75%`  = quantile(!!value_sym, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(across(-!!county_sym, ~ round(.x, 2))) %>%
    arrange(!!county_sym) %>%
    rename(County = !!county_sym)
}

escape_latex <- function(x) {
  x %>%
    str_replace_all("\\\\", "\\\\textbackslash{}") %>%
    str_replace_all("([#$%&_{}])", "\\\\\\1") %>%
    str_replace_all("\\^", "\\\\textasciicircum{}") %>%
    str_replace_all("~", "\\\\textasciitilde{}")
}

df_to_latex_rows <- function(df) {
  # ensure numeric columns are formatted with 2 decimals
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

make_table_S2_latex <- function(index_subset, file = NULL) {
  # Compute panels
  cap_tbl <- summarise_index(index_subset, county_norm, index_capacity)
  haz_tbl <- summarise_index(index_subset, county_norm, index_hazard)
  vul_tbl <- summarise_index(index_subset, county_norm, index_vulnerability)
  
  # Header blocks for longtable
  header_top <- paste0(
    "\\begin{longtable}{lrrrrr}
\\caption{Table S2. County-wise descriptive statistics by module (Mean, SD, 25th percentile, Median, 75th percentile).}\\\\
\\label{tab:S2_by_module}\\\\
\\toprule
\\textbf{County} & \\textbf{Mean} & \\textbf{SD} & \\textbf{25\\%} & \\textbf{Median} & \\textbf{75\\%} \\\\ 
\\midrule
\\endfirsthead

\\multicolumn{6}{c}{{\\bfseries Table S2 (continued)}} \\\\
\\toprule
\\textbf{County} & \\textbf{Mean} & \\textbf{SD} & \\textbf{25\\%} & \\textbf{Median} & \\textbf{75\\%} \\\\ 
\\midrule
\\endhead

\\midrule \\multicolumn{6}{r}{{Continued on next page}} \\\\
\\endfoot

\\bottomrule
\\endlastfoot
"
  )
  
  # Panel headers
  panel_a <- "\\multicolumn{6}{l}{\\textbf{(a) Capacity module}} \\\\\n\\midrule\n"
  panel_b <- "\\addlinespace[6pt]\n\\multicolumn{6}{l}{\\textbf{(b) Hazard module}} \\\\\n\\midrule\n"
  panel_c <- "\\addlinespace[6pt]\n\\multicolumn{6}{l}{\\textbf{(c) Vulnerability module}} \\\\\n\\midrule\n"
  
  # Build body rows
  body_a <- paste(df_to_latex_rows(cap_tbl), collapse = "\n")
  body_b <- paste(df_to_latex_rows(haz_tbl), collapse = "\n")
  body_c <- paste(df_to_latex_rows(vul_tbl), collapse = "\n")
  
  # Combine full LaTeX
  latex <- paste0(
    header_top,
    panel_a, body_a, "\n",
    panel_b, body_b, "\n",
    panel_c, body_c, "\n",
    "\\end{longtable}\n"
  )
  
  # Output
  if (!is.null(file)) {
    writeLines(latex, file, useBytes = TRUE)
  }
  latex
}

latex_code <- make_table_S2_latex(index_subset, file = "Table_S2_by_module.tex")
cat(latex_code)
