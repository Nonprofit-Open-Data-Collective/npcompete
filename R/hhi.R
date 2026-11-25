###---------------------------------------------------
###   Herfindahl–Hirschman Index
###---------------------------------------------------

#' Herfindahl–Hirschman Index
#'
#' Market concentration in each geography, subsector and year, with rollups
#' for all subsectors (`SUB = "ALL"`) and all geographies (`GEO = "ALL"`).
#'
#' @inheritParams npcompete-params
#' @param year Name of the year column, or `NULL` to pool all years.
#'
#' @return A tibble with `GEO`, `SUB`, `YEAR` (unless `year = NULL`), `N`, the
#'   summed resource and `HHI`. Markets whose resource total is zero get
#'   `HHI = NA`.
#'
#' @details HHI is the sum of squared market shares, where an organization's
#'   share is its resource divided by the market total (Thornton & Belski,
#'   2010). It ranges from 1/N to 1; a market with an HHI of 1 is a monopoly
#'   and values near 0 indicate many similar-sized competitors.
#'
#' @examples
#' toy <- data.frame( GEO = c( "A", "A", "B" ), SUB = "ART", YEAR = 2020,
#'                    TOTREV = c( 75, 25, 10 ) )
#' get_hhi( toy, geography = "GEO", subsector = "SUB", year = "YEAR" )
#'
#' @export
get_hhi <- function(df, geography = "CENSUS_CBSA_FIPS", subsector = "NTEEV2",
                    resource = "TOTREV", year = "TAXYEAR") {

  dat <- prep_market_data(df, geography, subsector, resource, year)
  out <- summarize_levels(dat, hhi_kernel)
  out$NHHI <- NULL
  name_total(out, resource)
}


#' Normalized Herfindahl–Hirschman Index
#'
#' HHI rescaled so that it runs from 0 to 1 whatever the number of
#' organizations, computed for each geography, subsector and year, with
#' rollups for all subsectors (`SUB = "ALL"`) and all geographies
#' (`GEO = "ALL"`).
#'
#' @inheritParams npcompete-params
#'
#' @return A tibble with `GEO`, `SUB`, `YEAR`, `N`, the summed resource, `HHI`
#'   and `NHHI`.
#'
#' @details `NHHI = (HHI - 1/N) / (1 - 1/N)`, where `N` is the number of
#'   organizations in the market. The formula is undefined for a single
#'   organization; such markets are monopolies and get `NHHI = 1`, consistent
#'   with `HHI`, `CR1` and `KWOKA`. Markets whose resource total is zero get
#'   `NA`. Filter on `N` to exclude single-organization markets.
#'
#' @examples
#' toy <- data.frame( GEO = c( "A", "A", "B" ), SUB = "ART", YEAR = 2020,
#'                    TOTREV = c( 75, 25, 10 ) )
#' get_nhhi( toy, geography = "GEO", subsector = "SUB", year = "YEAR" )
#'
#' @export
get_nhhi <- function(df, geography = "CENSUS_CBSA_FIPS", subsector = "NTEEV2",
                     resource = "TOTREV", year = "TAXYEAR") {

  dat <- prep_market_data(df, geography, subsector, resource, year)
  out <- summarize_levels(dat, hhi_kernel)
  name_total(out, resource)
}


# Per-market HHI and normalized HHI.
hhi_kernel <- function(x) {
  total <- sum(x)
  hhi <- if (total > 0) sum((x / total)^2) else NA_real_
  data.frame(N = length(x), TOTAL = total, HHI = hhi, NHHI = calc_nhhi(x))
}

# Normalized HHI for a vector of resource values: 1 for a single
# organization (monopoly), NA when the resource total is zero.
calc_nhhi <- function(x) {
  n <- length(x)
  total <- sum(x)
  if (total <= 0) return(NA_real_)
  if (n == 1) return(1)
  hhi <- sum((x / total)^2)
  (hhi - 1/n) / (1 - 1/n)
}
