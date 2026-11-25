###---------------------------------------------------
###   Count-based metrics: density per capita, size classes, age
###---------------------------------------------------

#' Nonprofit density per capita, overall and by size
#'
#' Counts organizations in each market and divides by population. When a
#' resource column is given, it also counts small and large organizations.
#' Every market in `pop_table` appears in every subsector, with zero counts
#' where it has no organizations.
#'
#' @inheritParams npcompete-params
#' @param pop_table Data frame of market populations with columns `GEO`,
#'   `YEAR` and `POP`, for example from [npc_get_population()].
#' @param resource Optional resource column used to classify organizations
#'   by size. `NULL` gives counts only.
#' @param small_max Organizations with a resource value below this are
#'   "small". The default is $100,000.
#' @param large_min Organizations with a resource value at or above this are
#'   "large". The default is $10 million.
#' @param per Population base for the rates (default 100,000).
#'
#' @return A tibble with `GEO`, `SUB`, `YEAR`, `POP`, `N` and
#'   `DENSITY_PER_100K`. With a resource, it also has `N_RES` (organizations
#'   with a non-missing value), `N_SMALL`, `N_LARGE`, `SMALL_DENSITY_PER_100K`,
#'   `LARGE_DENSITY_PER_100K`, `SMALL_SHARE` and `LARGE_SHARE` (shares of
#'   `N_RES`). Rows with `GEO = "ALL"` are national; their `POP` is the
#'   summed population of the markets in `pop_table`. Markets that have
#'   organizations but are missing from `pop_table` are kept with
#'   `POP = NA`.
#'
#' @details The column suffix stays `_PER_100K` for any `per`; the `per`
#'   value is stored in the `"per"` attribute.
#'
#' @examples
#' d <- get_density_per_capita( npo_panel_sample, geography = "COUNTY_FIPS",
#'                              year = "TAXYEAR",
#'                              pop_table = npc_get_population( "county", 2015:2019 ),
#'                              resource = "TOTREV" )
#' head( d[ d$SUB == "ALL", ] )
#'
#' @export
get_density_per_capita <- function(df, geography = "CENSUS_CBSA_FIPS",
                                   subsector = "NTEEV2", year = "TAXYEAR",
                                   pop_table, resource = NULL,
                                   small_max = 1e5, large_min = 1e7,
                                   per = 1e5) {

  dat <- prep_market_data(df, geography, subsector, resource, year,
                          drop_missing_res = FALSE)
  if (is.null(resource)) {
    dat$RES <- NA_real_
    kernel <- function(x) list(N = length(x))
    fill <- "N"
  } else {
    kernel <- function(x) list(N = length(x), N_RES = sum(!is.na(x)),
                               N_SMALL = sum(x < small_max, na.rm = TRUE),
                               N_LARGE = sum(x >= large_min, na.rm = TRUE))
    fill <- c("N", "N_RES", "N_SMALL", "N_LARGE")
  }

  out <- summarize_levels(dat, kernel)
  out <- fill_market_frame(out, pop_table, unique(dat$SUB), fill, dat$YEAR)

  out$DENSITY_PER_100K <- out$N / out$POP * per
  if (!is.null(resource)) {
    out$SMALL_DENSITY_PER_100K <- out$N_SMALL / out$POP * per
    out$LARGE_DENSITY_PER_100K <- out$N_LARGE / out$POP * per
    out$SMALL_SHARE <- ifelse(out$N_RES > 0, out$N_SMALL / out$N_RES, NA_real_)
    out$LARGE_SHARE <- ifelse(out$N_RES > 0, out$N_LARGE / out$N_RES, NA_real_)
  }
  attr(out, "per") <- per
  out
}


#' Median organization age
#'
#' Median years since the IRS ruling that granted exemption, among the
#' organizations in each market.
#'
#' @inheritParams npcompete-params
#' @param ruling Name of the ruling-year column.
#'
#' @return A tibble with `GEO`, `SUB`, `YEAR`, `N_AGE` (organizations with a
#'   valid ruling year) and `MEDIAN_AGE`. Ruling years after the tax year are
#'   treated as missing.
#'
#' @examples
#' a <- get_median_age( npo_panel_sample, geography = "COUNTY_FIPS",
#'                      year = "TAXYEAR", ruling = "ORG_RULING_YEAR" )
#' head( a[ a$SUB == "ALL", ] )
#'
#' @export
get_median_age <- function(df, geography = "CENSUS_CBSA_FIPS",
                           subsector = "NTEEV2", year = "TAXYEAR",
                           ruling = "RULING_YEAR") {
  if (!ruling %in% names(df)) stop("Column not found in df: ", ruling, call. = FALSE)
  age <- suppressWarnings(as.numeric(df[[year]]) - as.numeric(df[[ruling]]))
  age[age < 0] <- NA
  df$.npc_age <- age
  dat <- prep_market_data(df, geography, subsector, ".npc_age", year)
  summarize_levels(dat, function(x)
    list(N_AGE = length(x), MEDIAN_AGE = as.numeric(stats::median(x))))
}


# Expand market rows to every GEO x YEAR in pop_table and every subsector
# (plus "ALL"), zero-filling `fill` columns. National rows (GEO = "ALL")
# are kept and get the summed market population.
fill_market_frame <- function(out, pop_table, subs, fill, year_values) {
  if (!all(c("GEO", "YEAR", "POP") %in% names(pop_table)))
    stop("pop_table must have columns GEO, YEAR and POP.", call. = FALSE)

  frame <- data.frame(GEO = as.character(pop_table$GEO),
                      YEAR = pop_table$YEAR,
                      POP = as.numeric(pop_table$POP))
  frame <- frame[!is.na(frame$POP), ]
  frame$YEAR <- if (is.numeric(year_values)) as.numeric(frame$YEAR) else as.character(frame$YEAR)
  frame <- frame[frame$YEAR %in% unique(year_values), ]
  out$YEAR <- if (is.numeric(year_values)) as.numeric(out$YEAR) else as.character(out$YEAR)

  # Markets with organizations but no population (e.g. a CBSA code outside
  # the delineation used for pop_table) are kept with POP = NA.
  grid <- tidyr::expand_grid(frame, SUB = c(sort(unique(subs)), "ALL"))
  markets <- dplyr::full_join(grid, out[out$GEO != "ALL", ],
                              by = c("GEO", "SUB", "YEAR"))
  for (v in fill) markets[[v]][is.na(markets[[v]])] <- 0

  nat_pop <- stats::aggregate(POP ~ YEAR, data = frame, FUN = sum)
  national <- out[out$GEO == "ALL", ]
  national$POP <- nat_pop$POP[match(national$YEAR, nat_pop$YEAR)]

  res <- dplyr::bind_rows(markets, national)
  res <- dplyr::relocate(res, "GEO", "SUB", "YEAR", "POP")
  dplyr::arrange(res, GEO, SUB, YEAR)
}
