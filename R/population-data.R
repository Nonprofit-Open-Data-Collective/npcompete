#' County population, 2000-2025
#'
#' Annual population for U.S. counties on 2020 boundaries. Built from the
#' 2000, 2010 and 2020 decennial censuses and the Census Bureau's vintage
#' 2024 population estimates (2021-2024), with linear interpolation between
#' anchor years and 2025 carried forward from 2024. Built by
#' `data-raw/population/create-population-data.R`.
#'
#' @format A data frame with one row per county and year:
#' \describe{
#'   \item{COUNTY_FIPS}{Five-digit county FIPS code}
#'   \item{STATE_FIPS}{Two-digit state FIPS code}
#'   \item{COUNTY_NAME}{County name}
#'   \item{YEAR}{Year}
#'   \item{POP}{Population}
#' }
#' @source U.S. Census Bureau decennial census and population estimates, via
#'   `tidycensus`.
#' @seealso [pop_cbsa], [cbsa_county_xwalk], [npc_get_population()]
"pop_county"

#' CBSA population, 2000-2025
#'
#' Annual population for core-based statistical areas (metropolitan and
#' micropolitan), computed by summing [pop_county] over the counties in each
#' CBSA under the 2023 delineation, so every year uses the same boundaries.
#'
#' @format A data frame with one row per CBSA and year:
#' \describe{
#'   \item{CBSA_FIPS}{Five-digit CBSA code}
#'   \item{CBSA_NAME}{CBSA title}
#'   \item{YEAR}{Year}
#'   \item{POP}{Population}
#' }
#' @source U.S. Census Bureau; see [pop_county].
#' @seealso [cbsa_county_xwalk], [npc_get_population()]
"pop_cbsa"

#' County to CBSA crosswalk (2023 delineation)
#'
#' Counties that belong to a metropolitan or micropolitan statistical area
#' under the Census Bureau's July 2023 delineation. Counties outside any CBSA
#' are not listed.
#'
#' @format A data frame with one row per county:
#' \describe{
#'   \item{COUNTY_FIPS}{Five-digit county FIPS code}
#'   \item{COUNTY_NAME, STATE_NAME}{County and state names}
#'   \item{CBSA_FIPS, CBSA_NAME}{CBSA code and title}
#'   \item{CBSA_TYPE}{`"Metropolitan"` or `"Micropolitan"`}
#'   \item{CSA_FIPS, CSA_NAME}{Combined statistical area, if any}
#'   \item{CENTRAL_OUTLYING}{Whether the county is a central or outlying county}
#'   \item{DELINEATION}{Delineation vintage (2023)}
#' }
#' @source Census Bureau delineation file `list1_2023.xlsx`, as distributed by
#'   NBER (`cbsa2fipsxw`).
"cbsa_county_xwalk"
