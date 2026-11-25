###---------------------------------------------------
###   Kwoka Dominance Index
###---------------------------------------------------

#' Kwoka dominance index
#'
#' Kwoka's (1977) dominance index for each geography, subsector and year,
#' with rollups for all subsectors (`SUB = "ALL"`) and all geographies
#' (`GEO = "ALL"`). It measures how far the largest organizations pull ahead
#' of the next ones.
#'
#' @inheritParams npcompete-params
#'
#' @return A tibble with `GEO`, `SUB`, `YEAR`, `N`, the shares of the three
#'   largest organizations (`top1`, `top2`, `top3`; `NA` when the market has
#'   fewer organizations) and `KWOKA`.
#'
#' @details With market shares sorted from largest to smallest,
#'   \deqn{KWOKA = \sum_{i=1}^{N-1} (s_i - s_{i+1})^2}
#'   The index is 0 when all organizations have equal shares and approaches
#'   1 as one organization dominates. A market with a single organization
#'   gets `KWOKA = 1`; a market whose resource total is zero gets `NA`.
#'
#' @references Kwoka, J. E. (1977). Large firm dominance and price-cost
#'   margins in manufacturing industries. *Southern Economic Journal*, 44(1),
#'   183-189.
#'
#' @examples
#' toy <- data.frame( GEO = "A", SUB = "ART", YEAR = 2020,
#'                    TOTREV = c( 50, 30, 10, 10 ) )
#' get_kwoka( toy, geography = "GEO", subsector = "SUB", year = "YEAR" )
#'
#' @export
get_kwoka <- function(df, geography = "CENSUS_CBSA_FIPS", subsector = "NTEEV2",
                      resource = "TOTREV", year = "TAXYEAR") {

  dat <- prep_market_data(df, geography, subsector, resource, year)
  summarize_levels(dat, kwoka_kernel)
}

# Per-market Kwoka dominance index: sum of squared gaps between successive
# sorted shares.
kwoka_kernel <- function(x) {
  n <- length(x)
  total <- sum(x)
  if (total > 0) {
    share <- sort(x / total, decreasing = TRUE)
    k <- if (n == 1) 1 else sum(diff(share)^2)
  } else {
    share <- rep(NA_real_, n)
    k <- NA_real_
  }
  top <- share[1:3]                     # NA when fewer than three orgs
  list(N = n, top1 = top[1], top2 = top[2], top3 = top[3], KWOKA = k)
}
