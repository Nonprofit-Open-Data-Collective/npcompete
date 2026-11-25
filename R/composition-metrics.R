###---------------------------------------------------
###   Revenue composition: commercial share, fundraising
###---------------------------------------------------

#' Commercial share of a market
#'
#' How much a market relies on earned (program service) revenue rather than
#' contributions. Replaces the legacy [get_density_commercial()].
#'
#' @inheritParams npcompete-params
#' @param revenue Total revenue column.
#' @param program Program service revenue column.
#' @param cutoff An organization is "commercial" when program revenue is more
#'   than this share of its total revenue (default 0.5).
#'
#' @return A tibble with `GEO`, `SUB`, `YEAR`, `N_COMM` (organizations with
#'   positive total revenue and a non-missing program revenue),
#'   `COMMERCIAL_SHARE` (share of those organizations that are commercial)
#'   and `PROGRAM_REVENUE_SHARE` (program revenue as a share of total
#'   revenue in the market).
#'
#' @examples
#' cs <- get_commercial_share( npo_panel_sample, geography = "COUNTY_FIPS",
#'                             year = "TAXYEAR" )
#' head( cs[ cs$SUB == "ALL", ] )
#'
#' @export
get_commercial_share <- function(df, geography = "CENSUS_CBSA_FIPS",
                                 subsector = "NTEEV2", year = "TAXYEAR",
                                 revenue = "TOTREV", program = "REVPROG",
                                 cutoff = 0.5) {
  dat <- prep_market_data(df, geography, subsector, revenue, year,
                          extra = c(PROG = program))
  dat <- dat[dat$RES > 0 & !is.na(dat$PROG), ]
  summarize_levels(dat, cols = c("RES", "PROG"), function(rev, prog) {
    list(N_COMM = length(rev),
         COMMERCIAL_SHARE = if (length(rev)) mean(prog / rev > cutoff) else NA_real_,
         PROGRAM_REVENUE_SHARE = if (length(rev)) sum(pmax(prog, 0)) / sum(rev) else NA_real_)
  })
}


#' Professional fundraising in a market
#'
#' Use of paid fundraisers and the contributions they bring in. Replaces the
#' legacy [get_fundraising_efficiency()].
#'
#' @inheritParams npcompete-params
#' @param contributions Contributions and grants column.
#' @param fees Professional fundraising fees column.
#'
#' @return A tibble with `GEO`, `SUB`, `YEAR`, `N_FEE` (organizations that
#'   report the fee line), `FUNDRAISER_SHARE` (share of those paying
#'   professional fundraisers) and `FUNDRAISING_RATIO` (contributions per
#'   dollar of professional fundraising fees, among organizations that pay
#'   fees).
#'
#' @details Professional fundraising fees are reported only on the full Form
#'   990, so Form 990-EZ filers are excluded, and only a few percent of
#'   filers pay such fees. Markets with no fee-paying organizations get
#'   `FUNDRAISING_RATIO = NA`. The fee line does not include in-house
#'   fundraising staff, so this is not total fundraising efficiency.
#'
#' @examples
#' fr <- get_fundraising_ratio( npo_panel_sample, geography = "COUNTY_FIPS",
#'                              year = "TAXYEAR", fees = "FNDFEE" )
#' head( fr[ fr$SUB == "ALL", ] )
#'
#' @export
get_fundraising_ratio <- function(df, geography = "CENSUS_CBSA_FIPS",
                                  subsector = "NTEEV2", year = "TAXYEAR",
                                  contributions = "REVCONT", fees = "FNDFEE") {
  dat <- prep_market_data(df, geography, subsector, fees, year,
                          extra = c(CONT = contributions))
  summarize_levels(dat, cols = c("RES", "CONT"), function(fee, cont) {
    paid <- fee > 0
    list(N_FEE = length(fee),
         FUNDRAISER_SHARE = if (length(fee)) mean(paid) else NA_real_,
         FUNDRAISING_RATIO = if (any(paid)) sum(pmax(cont[paid], 0), na.rm = TRUE) / sum(fee[paid])
                             else NA_real_)
  })
}
