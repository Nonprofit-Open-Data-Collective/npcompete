# ===============================================================
# Build Consistent County-Level Population Dataset, 1990–2025
# ===============================================================
# Author: Jesse Lecy
# Purpose: Create a harmonized panel of county populations on 2025 boundaries
# ===============================================================

# -------------------------------
# STEP 1: Load Required Libraries
# -------------------------------
library(tidycensus)  # Census and population data
library(tigris)      # County shapefiles
library(dplyr)       # Data manipulation
library(tidyr)       # Filling and interpolation
library(stringr)     # String utilities
library(sf)          # Spatial data
library(lehdr)       # 
options(tigris_use_cache = TRUE)

YOUR_KEY <- source("census_api_key.R")

# NOTE: You need a Census API key (get one at https://api.census.gov/data/key_signup.html)
# census_api_key("YOUR_KEY", install = TRUE)
# readRenviron("~/.Renviron")

# -------------------------------
# STEP 2: Define County Reference
# -------------------------------
# Use 2020 counties as "2025" stable geography (latest consistent vintage)
counties_2020 <- 
  tigris::counties( year = 2020, cb = TRUE ) |>
  st_drop_geometry() |>
  transmute(
    GEOID = GEOID,
    NAME = NAME,
    STATEFP = STATEFP
  )

# -------------------------------
# STEP 3: Pull Population Data
# -------------------------------
# Decennial Census counts
vars <- c("1990" = "P001001", "2000" = "P001001", "2010" = "P001001", "2020" = "P1_001N")

get_dec <- function(year, var) {
  get_decennial(geography = "county", year = year, variables = var, cache_table = TRUE) |>
    rename(population = value) |>
    select(GEOID, population) |>
    mutate(year = year)
}

# pop1990 <- get_dec(1990, vars["1990"])
pop2000 <- get_dec(2000, vars["2000"])
pop2010 <- get_dec(2010, vars["2010"])
pop2020 <- get_dec(2020, vars["2020"])


# Annual Population Estimates: 2021–2023
# Using the most recent PEP "Vintage 2024" dataset

pop_estimates <- 
  tidycensus::get_estimates(
    geography = "county",
    product = "population",
    vintage = 2024,
    time_series = TRUE
  ) |>
  dplyr::filter(
    year %in% 2021:2024,
    variable == "POPESTIMATE" ) |>
  dplyr::select( GEOID, year, population = value )



#  table( pop_estimates$year )
#  2021 2022 2023 2024 
#  3144 3144 3144 3144 


# -------------------------------
# STEP 4: Combine and Harmonize
# -------------------------------

pop_all <- 
  bind_rows( pop2000, pop2010, pop2020, pop_estimates ) |>
  right_join( counties_2020, by = "GEOID" ) |>   # retain only stable 2020 counties
  arrange( GEOID, year )


#  table( pop_all$year )
#  2000 2010 2020 2021 2022 2023 2024 
#  3211 3217 3221 3135 3135 3135 3135 


# -------------------------------
# STEP 5: Fill and Interpolate
# -------------------------------
# Fill missing years  by linear interpolation
complete_panel <- pop_all |>
  group_by(GEOID) |>
  complete(year = 2000:2025) |>
  arrange(GEOID, year) |>
  # impute population
  mutate(population = if (sum(!is.na(population)) > 1)
                      approx(year, population, year, rule = 2)$y
                      else population ) |>
  # fills county name and state
  fill( NAME, STATEFP, .direction = "downup") |>   
  ungroup()

table( complete_panel$year )


# -------------------------------
# STEP 6: Attach County Names
# -------------------------------
final_data <- 
  complete_panel |>
  left_join( counties_2020, by = c("GEOID","STATEFP","NAME") ) |>
  dplyr::select( GEOID, STATEFP, NAME, year, population )

table( final_data$year )

mean( is.na(final_data$population )) 
# [1] 0.004173752



# -------------------------------
# STEP 7: Export to CSV
# -------------------------------
write.csv(final_data, "county_population_2000_2025.csv", row.names = FALSE)

# Optional: preview
print(head(final_data))
cat("✅ county_population_1990_2025.csv successfully created.\n")