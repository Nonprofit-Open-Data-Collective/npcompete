###---------------------------------------------------
###   Concentration Ratio
###---------------------------------------------------

#' Concentration ratios CR1 to CRk
#'
#' Share of a market's total resource held by its largest organizations,
#' computed for each geography, subsector and year, with rollups for all
#' subsectors (`SUB = "ALL"`) and all geographies (`GEO = "ALL"`).
#'
#' @inheritParams npcompete-params
#' @param numbers Integer vector of CR levels to calculate, for example
#'   `c(1, 4, 8)`. Values above 8 produce a warning.
#' @param as_percent If `TRUE`, ratios are returned on a 0-100 scale instead
#'   of 0-1.
#'
#' @return A tibble with `GEO`, `SUB`, `YEAR`, `N`, the summed resource and one
#'   column per requested ratio (`CR1`, `CR2`, ...). Markets with k or fewer
#'   organizations get `CRk = 1`.
#'
#' @details CRk is the sum of the resource held by the k largest organizations
#'   divided by the market total.
#'
#' @examples
#' toy <- data.frame( GEO = "A", SUB = "ART", YEAR = 2020,
#'                    TOTREV = c( 50, 30, 10, 10 ) )
#' get_cr_all( toy, geography = "GEO", subsector = "SUB", year = "YEAR",
#'             numbers = 1:3 )
#'
#' @export
get_cr_all <- function(df,
                       geography  = "CENSUS_CBSA_FIPS",
                       subsector  = "NTEEV2",
                       resource   = "TOTREV",
                       year       = "TAXYEAR",
                       numbers    = 1:8,
                       as_percent = FALSE) {

  if (any(numbers > 8))
    warning("Values > 8 uncommon; CR8 is a conventional upper limit.")

  dat <- prep_market_data(df, geography, subsector, resource, year)
  out <- summarize_levels(dat, function(x) cr_kernel(x, numbers, as_percent))
  name_total(out, resource)
}


#' Concentration ratio (CRk)
#'
#' Convenience wrapper around [get_cr_all()] for a single CR level.
#'
#' @inheritParams get_cr_all
#' @param number Number of top organizations to include, for example `4` for
#'   CR4.
#'
#' @return A tibble with `GEO`, `SUB`, `YEAR`, `N`, the summed resource and
#'   `CR<number>`.
#'
#' @examples
#' toy <- data.frame( GEO = "A", SUB = "ART", YEAR = 2020,
#'                    TOTREV = c( 50, 30, 10, 10 ) )
#' get_cr( toy, geography = "GEO", subsector = "SUB", year = "YEAR", number = 2 )
#'
#' @export
get_cr <- function(df, geography = "CENSUS_CBSA_FIPS", subsector = "NTEEV2",
                   resource = "TOTREV", year = "TAXYEAR", number = 4) {
  get_cr_all(df, geography, subsector, resource, year, numbers = number)
}


# Per-market concentration ratios for each k in `numbers`.
cr_kernel <- function(x, numbers, pct = FALSE) {
  n <- length(x)
  total <- sum(x)
  sorted <- sort(x, decreasing = TRUE)
  vals <- vapply(numbers, function(k) {
    ratio <- if (n <= k) 1
             else if (total > 0) sum(utils::head(sorted, k)) / total
             else NA_real_
    if (pct) 100 * ratio else ratio
  }, numeric(1))
  c(list(N = n, TOTAL = total), stats::setNames(as.list(vals), paste0("CR", numbers)))
}
