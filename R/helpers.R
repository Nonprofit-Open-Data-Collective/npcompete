###---------------------------------------------------
###   Internal helpers shared by the metric functions
###---------------------------------------------------

# Default rule for turning a subsector column into market subsectors: for
# NTEEV2 codes ("HMS-N67-RG") use the three-letter major group ("HMS").
# Missing or blank subsectors become "UNK".
make_subsector <- function(x, subsector) {
  x <- as.character(x)
  if (identical(subsector, "NTEEV2")) x <- substr(x, 1, 3)
  x[is.na(x) | x == ""] <- "UNK"
  x
}

# Pull the analysis columns out of an org-level data frame into a standard
# layout: GEO, SUB, YEAR (unless year = NULL), RES (and POP if requested).
#
# - Negative resource values are bottom-coded to zero.
# - Rows with a missing resource (or year) are dropped when
#   `drop_missing_res = TRUE`, as the concentration metrics require.
# - GEO is kept as character with NA for missing geography. Market-level
#   rows later drop NA geographies, but national rollups keep those orgs.
prep_market_data <- function(df, geography, subsector, resource = NULL,
                             year = NULL, population = NULL,
                             drop_missing_res = TRUE) {

  need <- c(geography, subsector, resource, year, population)
  missing_cols <- setdiff(need, names(df))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in df: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }

  geo <- as.character(df[[geography]])
  geo[geo == ""] <- NA
  dat <- data.frame(
    GEO = geo,
    SUB = make_subsector(df[[subsector]], subsector),
    stringsAsFactors = FALSE
  )
  if (!is.null(year))       dat$YEAR <- df[[year]]
  if (!is.null(resource))   dat$RES  <- bottomcode(suppressWarnings(as.numeric(df[[resource]])))
  if (!is.null(population)) dat$POP  <- suppressWarnings(as.numeric(df[[population]]))

  if (drop_missing_res && !is.null(resource)) dat <- dat[!is.na(dat$RES), , drop = FALSE]
  if (!is.null(year)) dat <- dat[!is.na(dat$YEAR), , drop = FALSE]

  dat
}

# Apply a per-market summary function at three levels and stack the results:
#   GEO x SUB x YEAR   (markets; rows with missing GEO are dropped)
#   GEO x YEAR         (SUB = "ALL")
#   SUB x YEAR         (GEO = "ALL"; includes orgs with missing GEO)
#
# `.f` takes the resource vector for one group and returns a one-row
# data frame of metric columns.
summarize_levels <- function(dat, .f) {

  yr <- if ("YEAR" %in% names(dat)) "YEAR" else character(0)

  run <- function(d, keys) {
    d |>
      dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
      dplyr::summarize(.f(RES), .groups = "drop")
  }

  markets <- dat[!is.na(dat$GEO), , drop = FALSE]

  by_geo_sub <- run(markets, c("GEO", "SUB", yr))
  by_geo     <- run(markets, c("GEO", yr));  by_geo$SUB <- "ALL"
  by_sub     <- run(dat,     c("SUB", yr));  by_sub$GEO <- "ALL"

  out <- dplyr::bind_rows(by_geo_sub, by_geo, by_sub)
  out <- dplyr::relocate(out, dplyr::all_of(c("GEO", "SUB", yr)))
  dplyr::arrange(out, dplyr::across(dplyr::all_of(c("GEO", "SUB", yr))))
}

# Rename the generic TOTAL column to the resource name (e.g. TOTREV).
name_total <- function(out, resource) {
  names(out)[names(out) == "TOTAL"] <- resource
  out
}

#' Flag years with incomplete data
#'
#' Panels built from IRS filings fill in over several years, so the most
#' recent tax years (and the first years of a panel) hold only part of the
#' population. A year is flagged when its row count is below `threshold`
#' times the median row count across years.
#'
#' @param x Vector of years, one element per organization-year row.
#' @param threshold Share of the median yearly count below which a year is
#'   flagged.
#'
#' @return A data frame with `YEAR`, `N`, `SHARE_OF_MEDIAN` and `INCOMPLETE`.
#'
#' @examples
#' yrs <- c( rep( 2019, 100 ), rep( 2020, 98 ), rep( 2021, 101 ), rep( 2022, 40 ) )
#' check_year_completeness( yrs )
#'
#' @export
check_year_completeness <- function(x, threshold = 0.8) {
  tab <- table(x[!is.na(x)])
  n <- as.numeric(tab)
  share <- n / stats::median(n)
  data.frame(
    YEAR = utils::type.convert(names(tab), as.is = TRUE),
    N = n,
    SHARE_OF_MEDIAN = round(share, 3),
    INCOMPLETE = share < threshold
  )
}

# Deprecation notice for functions built on the legacy NCCS core-file layout.
legacy_warning <- function(fn) {
  .Deprecated(msg = sprintf(paste(
    "%s() uses the legacy NCCS core-file layout (MSA_NECH, NTMAJ12, FIPS)",
    "and is deprecated. Use get_scale_free_density() for density; the other",
    "legacy metrics will be rebuilt on the shared metric framework."), fn))
}
