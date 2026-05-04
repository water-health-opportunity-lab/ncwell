# ncwell

Code repository for the REACH Center pilot project  
_Data-Driven Solutions to Mitigate the Impact of Hurricanes on Private Well Drinking Water Quality and Human Health_

This repository contains the analysis workflow for developing hazard, physical vulnerability, and social capacity indices for private well contamination risk in western North Carolina, along with model validation, figure generation, and supporting-information tables.

## Project Structure

```text
ncwell/
├── LICENSE
├── README.md
├── ncwell.Rproj
├── renv.lock
├── renv/
├── data/                         # local input/output data; do not commit large raw data
└── script/
    ├── 1_access_data/            # data acquisition scripts
    ├── 2_data_wrangling/         # cleaning, harmonization, grid construction, imputation
    ├── 3_exploratory_data_analysis/
    ├── 4_statistical_analysis/   # PCA, index construction, risk mapping
    ├── 5_model_validation/       # validation, robustness, selection-bias analyses
    └── 6_figures_tables/         # manuscript and SI figures/tables
```

## Script Overview

- `script/1_access_data/`: scripts to pull or assemble upstream datasets, including ACS and capacity-related inputs.
- `script/2_data_wrangling/`: preprocessing scripts for hazard, vulnerability, and capacity layers, including common-grid setup and KNN imputation.
- `script/3_exploratory_data_analysis/`: Quarto notebooks for module-specific exploratory analysis.
- `script/4_statistical_analysis/`: Quarto notebooks for PCA-based dimension reduction, index construction, and risk mapping.
- `script/5_model_validation/`: validation and robustness workflows, including hybrid supervised-unsupervised model assessment and selection-bias screening.
- `script/6_figures_tables/`: scripts that generate manuscript figures and supporting-information tables such as `figure3.R`, `figureS1.R`, `figureS2.R`, `tableS8.R`, and `tableS10-to-S14.R`.

## Getting Started

1. Clone the repository and open `ncwell.Rproj` in RStudio.
2. Restore the project library with `renv::restore()`.
3. Confirm the environment with `renv::status()`.
4. Run scripts in workflow order, starting from data access and wrangling, then exploratory analysis, statistical analysis, validation, and finally figures/tables.

Example:

```r
renv::restore()
renv::status()
```

## Requirements

- R
- RStudio (recommended)
- System libraries required by spatial packages such as `sf`
- A working LaTeX installation if you want to compile manuscript-ready `.tex` outputs into PDF

## Reproducibility

This project uses `renv` to manage package versions. The lockfile records the package state used for the analysis. If you install or update packages intentionally, snapshot the environment with:

```r
renv::snapshot()
```

## Data and Secrets

- Keep API keys such as `CENSUS_API_KEY` in `~/.Renviron`.
- Do not commit `.Renviron`, large raw data files, or derived data that should remain local.

Example `~/.Renviron` entry:

```text
CENSUS_API_KEY=your_key_here
```

## Git Workflow

This repository follows a feature-branch workflow.

```text
git checkout develop
git pull origin develop
git checkout -b feat/short-description
git push -u origin feat/short-description
```

Submit a pull request into `develop` and request review from the project maintainers.

## Project Status

This project is under active development as part of the REACH Center pilot grant.
