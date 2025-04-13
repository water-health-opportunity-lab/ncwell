# ----------------------------------------
# R Script to Download and Process 2023 ACS Data for North Carolina
# Indicators:
#   - % Minorities (non-White, including Hispanic/Latino)
#   - % Limited English Speakers
#   - % Single-Parent Households (with children <18, no spouse present)
#
# Author: Xindi Hu
# Last edited: 2025-04-13
# ----------------------------------------

# Load packages
library(tidycensus)
library(tidyverse)

# Set your Census API key, save your API key in .Renviron, don't save it in script.
census_api_key(Sys.getenv("CENSUS_API_KEY"), install = FALSE)

# Set year and geography
year <- 2023
state <- "NC"
geo <- "tract"

# Variables from ACS
vars <- c(
  total_pop = "B03002_001",
  black_pop = "B03002_004",
  native_pop = "B03002_003",
  asian_pop = "B03002_005",
  nhpi_pop = "B03002_006",
  other_pop = "B03002_007",
  multi_pop = "B03002_008",
  hispanic_pop = "B03002_012",
  
  total_households = "B11003_001",
  female_no_spouse = "B11003_010",
  male_no_spouse = "B11003_007",
  
  total_speakers = "C16002_001",
  limited_english = "C16002_004"
)

# Download data
acs_data <- get_acs(
  geography = geo,
  state = state,
  year = year,
  survey = "acs5",
  variables = vars,
  output = "wide"
)

# Calculate indicators
acs_processed <- acs_data %>%
  transmute(
    GEOID,
    NAME,
    pct_minority = 100 * (
      black_popE + native_popE + asian_popE +
        nhpi_popE + other_popE + multi_popE + hispanic_popE
    ) / total_popE,
    
    pct_limited_english = 100 * (limited_englishE / total_speakersE),
    
    pct_single_parent = 100 * (female_no_spouseE + male_no_spouseE) / total_householdsE
  )

# View results
glimpse(acs_processed)
