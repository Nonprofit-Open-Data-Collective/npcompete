###---------------------------------------------------
###   Build bundled population and crosswalk datasets
###---------------------------------------------------
#
# Sources (copied into this folder):
#   county_population_2000_2025.csv  built by build-population-panel.R:
#       decennial counts 2000/2010/2020 and PEP vintage 2024 estimates
#       (2021-2024) on 2020 county boundaries, linearly interpolated, with
#       2025 carried forward from 2024.
#   cbsa_population_2000_2025.csv    the county panel summed to CBSAs with
#       the 2023 delineation.
#   cbsa2fipsxw.csv                  NBER copy of the Census 2023 CBSA
#       delineation file (list1_2023.xlsx).
#
# Run from the package root: source("data-raw/population/create-population-data.R")

library( dplyr )

p <- "data-raw/population/"

pop_county <-
  read.csv( paste0( p, "county_population_2000_2025.csv" ),
            colClasses = c( GEOID = "character", STATEFP = "character" ) ) |>
  filter( ! is.na( population ) ) |>                 # territories without estimates
  transmute( COUNTY_FIPS = GEOID,
             STATE_FIPS  = STATEFP,
             COUNTY_NAME = NAME,
             YEAR        = as.integer( year ),
             POP         = population ) |>
  arrange( COUNTY_FIPS, YEAR )

pop_cbsa <-
  read.csv( paste0( p, "cbsa_population_2000_2025.csv" ),
            colClasses = c( cbsacode = "character" ) ) |>
  filter( ! is.na( population ), cbsacode != "" ) |>
  transmute( CBSA_FIPS = cbsacode,
             CBSA_NAME = cbsatitle,
             YEAR      = as.integer( year ),
             POP       = population ) |>
  arrange( CBSA_FIPS, YEAR )

cbsa_county_xwalk <-
  read.csv( paste0( p, "cbsa2fipsxw.csv" ), colClasses = "character" ) |>
  transmute( COUNTY_FIPS      = paste0( fipsstatecode, fipscountycode ),
             COUNTY_NAME      = countycountyequivalent,
             STATE_NAME       = statename,
             CBSA_FIPS        = cbsacode,
             CBSA_NAME        = cbsatitle,
             CBSA_TYPE        = ifelse( grepl( "^Metro", metropolitanmicropolitanstatis ),
                                        "Metropolitan", "Micropolitan" ),
             CSA_FIPS         = na_if( csacode, "" ),
             CSA_NAME         = na_if( csatitle, "" ),
             CENTRAL_OUTLYING = centraloutlyingcounty,
             DELINEATION      = 2023L ) |>
  arrange( COUNTY_FIPS )

stopifnot( ! anyDuplicated( cbsa_county_xwalk$COUNTY_FIPS ) )

usethis::use_data( pop_county, pop_cbsa, cbsa_county_xwalk,
                   overwrite = TRUE, compress = "xz" )
