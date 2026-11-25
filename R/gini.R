###---------------------------------------------------
###   Gini Coefficient
###---------------------------------------------------

#' Gini coefficient
#'
#' Inequality of the resource distribution in each geography, subsector and
#' year, with rollups for all subsectors (`SUB = "ALL"`) and all geographies
#' (`GEO = "ALL"`).
#'
#' @inheritParams npcompete-params
#' @param year Name of the year column, or `NULL` to pool all years.
#'
#' @return A tibble with `GEO`, `SUB`, `YEAR` (unless `year = NULL`), `N`, the
#'   summed resource and `GINI`. Markets with a single organization get
#'   `GINI = 1`.
#'
#' @details The Gini coefficient is a widely used measure of inequality
#'   (Dorfman, 1979). Low values indicate less inequality and more
#'   competition; high values indicate greater inequality. Seaman et al.
#'   (2014) used it alongside HHI and suggested reading values below 0.5 as
#'   reasonable equality, 0.5 to 0.75 as modest inequality, and above 0.75 as
#'   high inequality. Computed with [DescTools::Gini()].
#'
#' @examples
#' toy <- data.frame( GEO = c( "A", "A", "A", "B" ), SUB = "ART", YEAR = 2020,
#'                    TOTREV = c( 10, 20, 70, 5 ) )
#' get_gini( toy, geography = "GEO", subsector = "SUB", year = "YEAR" )
#'
#' @export
get_gini <- function(df, geography = "CENSUS_CBSA_FIPS", subsector = "NTEEV2",
                     resource = "TOTREV", year = "TAXYEAR") {

  dat <- prep_market_data(df, geography, subsector, resource, year)
  out <- summarize_levels(dat, gini_kernel)
  name_total(out, resource)
}

# Per-market Gini coefficient.
gini_kernel <- function(x) {
  n <- length(x)
  g <- if (n == 1) 1 else suppressWarnings(DescTools::Gini(x))
  if (is.nan(g)) g <- NA_real_
  data.frame(N = n, TOTAL = sum(x), GINI = g)
}
