# ----------------------------------------
# R Script to Download and Process 2023 ACS Data for North Carolina
#
# Tract Level Indicators:
#   - % Below 150% poverty
#   - % Women who gave birth in the past 12 months
#   - % Insured
#   - % Born in the US
#   - % Lived 1+ years in the same house
#   - % Workers >= 16 years who are government workers
#
# Block Group Level Indicators:
#   - % Minorities (non-White, including Hispanic/Latino)
#   - % Limited English Speakers
#   - % Single-Parent Households (with children <18, no spouse present)
#   - % Unemployed
#   - % No high school diploma
#   - % Women
#   - % > 65 years
#   - % < 17 years
#   - % Owner occupied households
#   - % Workers >= 16 years who commute to work by transit, walking, or cycling
#   - % Households with > 1 occupant per room
#   - % Households with complete plumbing
#   - % Homes built after 2010
#   - % Workers >= 16 years with a motor vehicle
#   - % Households with internet
#   - % Households with a computer
#   - # Population size
#   - # Housing units
#   - # Households
#
# Author: Jennifer Zhang
# Last edited: 2025-04-25
# ----------------------------------------

# Load packages
library(tidycensus)
library(tidyverse)
library(sf)

# Set your Census API key, save your API key in .Renviron, don't save it in script.
census_api_key(Sys.getenv("CENSUS_API_KEY"), install = FALSE)

# Set year and geography
year <- 2023
state <- "NC"

# Tract Level Variables from ACS
tract_vars <- c(
  # pct_poverty
  total_determined_poverty = "B06012_001",
  below_100_percent_poverty = "B06012_002",
  btwn_100_to_149_percent_poverty = "B06012_003",
  
  # pct_women_gave_birth
  total_women_15_to_50 = "B13002_001",
  birth_in_past_year = "B13002_002",
  
  # pct_insured
  total_civ_pop = "B27020_001",
  insured_native = "B27020_003",
  insured_naturalized = "B27020_009",
  insured_noncitizen = "B27020_014",
  
  # pct_us_born
  total_tract_pop = "B05001_001",
  born_in_us = "B05001_002",
  
  # pct_no_move
  total_over_1 = "B07003_001",
  same_house = "B07003_004",
  
  # pct_gov_workers
  total_workers_over_16 = "B08128_001",
  local_gov = "B08128_006",
  state_gov = "B08128_007",
  fed_gov = "B08128_008"
)

# Download tract data
acs_tract_data <- get_acs(
  geography = "tract",
  state = state,
  year = year,
  survey = "acs5",
  variables = tract_vars,
  output = "wide"
)

# Calculate indicators at tract level
acs_processed_tract <- acs_tract_data %>%
  transmute(
    GEOID,
    NAME,
    pct_poverty = 100 * (below_100_percent_povertyE + btwn_100_to_149_percent_povertyE) / total_determined_povertyE,
    
    pct_women_gave_birth = 100 * (birth_in_past_yearE / total_women_15_to_50E),
    
    pct_insured = 100 * (insured_nativeE + insured_naturalizedE + insured_noncitizenE) / total_civ_popE,
    
    pct_us_born = 100 * (born_in_usE / total_tract_popE),
    
    pct_no_move = 100 * (same_houseE / total_over_1E),
    
    pct_gov_workers = 100 * (local_govE + state_govE + fed_govE) / total_workers_over_16E,
  )

# View block group results
View(acs_processed_tract)
write.csv(acs_processed_tract, "acs_tract.csv")

# Block Group Level Variables from ACS
block_group_vars <- c(
  # pct_minority
  total_pop = "B02001_001",
  black_pop = "B02001_003",
  native_pop = "B02001_004",
  asian_pop = "B02001_005",
  nhpi_pop = "B02001_006",
  other_pop = "B02001_007",
  multi_pop = "B02001_008",

  # pct_single_parent
  total_families = "B11003_001",
  male_no_spouse = "B11003_010",
  female_no_spouse = "B11003_016",
  
  # pct_limited_english
  total_speakers = "C16002_001",
  limited_english = "C16002_004",
  
  # pct_unemployed
  total_civilian_labor_force = "B23025_003",
  unemployed = "B23025_005",
  
  # pct_less_than_hs
  total_over_25 = "B28006_001",
  less_than_hs = "B28006_002",
  
  # pct_women (use total_pop)
  female = "B01001_026",
  
  # pct_over_65
  total_pop_above_5 = "B16004_001",
  over_65_years = "B16004_046",
  
  # pct_under_17 (use total_pop_above_5)
  btwn_5_to_17_years = "B16004_002",
  
  # pct_owner_occupied
  total_occupied_housing = "B25003_001",
  owner_occupied = "B25003_002",
  
  # pct_active_commuting
  total_workers_over_16 = "B08301_001",
  public_transit = "B08301_010",
  bicycle = "B08301_018",
  walked = "B08301_019",
  
  # pct_crowded_housing (use total_occupied_housing)
  # o - owner; r - renter; number specifies occupants per room
  o_btwn_1.01_1.50 = "B25014_005",
  o_btwn_1.51_2.00 = "B25014_006",
  o_above_2.01 = "B25014_007",
  r_btwn_1.01_1.50 = "B25014_011",
  r_btwn_1.51_2.00 = "B25014_012",
  r_above_2.01 = "B25014_013",
  
  # pct_new_home
  total_housing_units = "B25034_001",
  built_2020_later = "B25034_002",
  built_2010_2019 = "B25034_003",
  
  # pct_plumbing (use total_housing)
  complete_plumbing = "B25047_002",
  
  # pct_no_vehicle (use total_occupied_housing)
  o_no_vehicle = "B25044_003",
  r_no_vehicle = "B25044_010",
  
  # pct_no_internet (use total_households)
  total_households = "B28002_001",
  no_internet = "B28002_013",
  
  # pct_no_computer (use total_households)
  no_computer = "B28001_011"
)

# Download block group data
acs_block_group_data <- get_acs(
  geography = "block group",
  state = state,
  year = year,
  survey = "acs5",
  variables = block_group_vars,
  output = "wide",
  geometry = TRUE
)

acs_block_group_data <- st_as_sf(acs_block_group_data)

# Calculate indicators at block group level
acs_processed_block_group <- acs_block_group_data %>%
  transmute(
    GEOID,
    NAME,
    pct_minority = 100 * (
      black_popE + native_popE + asian_popE +
        nhpi_popE + other_popE + multi_popE
    ) / total_popE,
    
    pct_limited_english = 100 * (limited_englishE / total_speakersE),
    
    pct_single_parent = 100 * (female_no_spouseE + male_no_spouseE) / total_familiesE,
    
    pct_unemployed = 100 * (unemployedE / total_civilian_labor_forceE),
    
    pct_no_college = 100 * (less_than_hsE / total_over_25E), 

    pct_women = 100 * (femaleE / total_popE),
    
    pct_over_65 = 100 * (over_65_yearsE / total_pop_above_5E),
    
    pct_under_17 = 100 * (btwn_5_to_17_yearsE / total_pop_above_5E),

    pct_owner_occupied = 100 * (owner_occupiedE / total_occupied_housingE),
    
    pct_active_commuting = 100 * (public_transitE + bicycleE + walkedE) / total_workers_over_16E,

    pct_crowded_housing = 100 * (
      o_btwn_1.01_1.50E + o_btwn_1.51_2.00E + o_above_2.01E +
      r_btwn_1.01_1.50E + r_btwn_1.51_2.00E + r_above_2.01E
      ) / total_occupied_housingE,

    pct_plumbing = 100 * (complete_plumbingE / total_housing_unitsE),
    
    pct_new_home = 100 * (built_2020_laterE + built_2010_2019E) / total_housing_unitsE,
    
    pct_no_vehicle = 100 * (o_no_vehicleE + r_no_vehicleE) / total_occupied_housingE,
    
    pct_no_internet = 100 * (no_internetE / total_householdsE),
    
    pct_no_computer = 100 * (no_computerE / total_householdsE),
    
    pop_density = total_popE / as.numeric(st_area(.) / 1e6), 
    
    housing_units_density = total_housing_unitsE / as.numeric(st_area(.) / 1e6),
    
    households_density = total_householdsE / as.numeric(st_area(.) / 1e6)
  )

# View block group results
glimpse(acs_processed_block_group)
write.csv(acs_processed_block_group, "acs_block_group.csv")