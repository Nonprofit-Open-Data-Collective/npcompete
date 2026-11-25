###---------------------------------------------------
###   Get all metrics
###---------------------------------------------------

#' Compute all competition metrics in one table
#'
#' Runs [get_gini()], [get_nhhi()], [get_kwoka()], [get_cr_all()] and
#' [get_scale_free_density()] and joins the results, one row per geography,
#' subsector and year (plus `ALL` rollups).
#'
#' @inheritParams npcompete-params
#' @inheritParams get_scale_free_density
#' @param cr_n Integer vector of concentration ratio levels to compute.
#' @param drop_incomplete_years If `TRUE` (the default), years flagged by
#'   [check_year_completeness()] are dropped with a message before any metric
#'   is computed.
#'
#' @return A tibble with `GEO`, `SUB`, `YEAR`, `N`, `POP`, the summed
#'   resource, `LOG_NPO`, `LOG_POP`, `LOG_<resource>`, `DENSITY_PCTL_N`,
#'   `DENSITY_PCTL_R`, `GINI`, `HHI`, `NHHI`, `KWOKA` and `CR1` to `CR8`.
#'   Market cells with no organizations (from the density frame) have
#'   `N = 0` and `NA` concentration metrics.
#'
#' @examples
#' m <- get_all_metrics( npo_panel_sample,
#'                       geography = "COUNTY_FIPS",
#'                       population = "POP_COUNTY",
#'                       resource = "TOTREV" )
#' head( m )
#'
#' @export
get_all_metrics <- function(df,
                            geography  = "CENSUS_CBSA_FIPS",
                            population = "POP_METRO",
                            subsector  = "NTEEV2",
                            resource   = "TOTREV",
                            year       = "TAXYEAR",
                            cr_n       = 1:8,
                            pop_table  = NULL,
                            drop_incomplete_years = TRUE) {

  # --- Drop partial years --------------------------------------------------
  if (drop_incomplete_years) {
    yc <- check_year_completeness(df[[year]])
    bad <- yc$YEAR[yc$INCOMPLETE]
    if (length(bad) > 0) {
      message("Dropping incomplete years: ", paste(bad, collapse = ", "),
              " (set drop_incomplete_years = FALSE to keep them).")
      df <- df[!df[[year]] %in% bad, , drop = FALSE]
    }
  }

  # --- Run all individual metrics ------------------------------------------
  message("Calculating Gini...")
  dat_gini  <- get_gini(df, geography, subsector, resource, year)

  message("Calculating normalized HHI...")
  dat_nhhi  <- get_nhhi(df, geography, subsector, resource, year)

  message("Calculating Kwoka index...")
  dat_kwoka <- get_kwoka(df, geography, subsector, resource, year)

  message("Calculating concentration ratios...")
  dat_cr    <- get_cr_all(df, geography, subsector, resource, year, numbers = cr_n)

  message("Calculating nonprofit density...")
  dat_den   <- get_scale_free_density(df, geography, population, subsector,
                                      year, resource, pop_table = pop_table)
  dat_den   <- dplyr::select(dat_den, -model_n, -model_r, -N, -dplyr::all_of(resource))

  # --- Merge all results ---------------------------------------------------
  # Joins use every shared column (GEO, SUB, YEAR, and N / resource where
  # both tables have them).
  join_all <- function(x, y) dplyr::full_join(x, y, by = intersect(names(x), names(y)))

  out <- dat_gini |>
    join_all(dat_nhhi) |>
    join_all(dat_kwoka) |>
    join_all(dat_cr) |>
    dplyr::full_join(dat_den, by = c("GEO", "SUB", "YEAR"))

  out$N[is.na(out$N)] <- 0L

  # --- Tidy up column order ------------------------------------------------
  log_resource <- paste0("LOG_", resource)
  metric_cols <- c(
    "N", "POP", resource, "LOG_NPO", "LOG_POP", log_resource,
    "DENSITY_PCTL_N", "DENSITY_PCTL_R", "GINI", "HHI", "NHHI", "KWOKA",
    paste0("CR", cr_n)
  )
  existing_cols <- intersect(metric_cols, names(out))
  out <- dplyr::select(out, GEO, SUB, YEAR, dplyr::all_of(existing_cols))
  dplyr::arrange(out, GEO, SUB, YEAR)
}
