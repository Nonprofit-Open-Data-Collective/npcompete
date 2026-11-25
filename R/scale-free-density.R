###---------------------------------------------------
###   Scale-free density
###---------------------------------------------------

#' Scale-free nonprofit density
#'
#' Measures whether a market has more or fewer nonprofits (and more or less of
#' a resource) than expected for its population. Within each subsector and
#' year, the logged count of nonprofits and the logged resource total are
#' regressed on logged population across markets. The residuals are converted
#' back to a proportional difference from the expected value.
#'
#' @inheritParams npcompete-params
#' @param pop_table Optional data frame of market populations with columns
#'   `GEO`, `YEAR` and `POP`. Supply it so that markets with no organizations
#'   at all are still included. When `NULL`, market populations are taken from
#'   the `population` column of `df`, so only markets with at least one
#'   organization (in any subsector) are included.
#'
#' @return A tibble with one row per `GEO`, `SUB` and `YEAR`, containing `N`,
#'   `POP`, the summed resource, `LOG_NPO`, `LOG_POP`, `LOG_<resource>`,
#'   `DENSITY_PCTL_N`, `DENSITY_PCTL_R` (both capped at 2) and the fitted
#'   models in the list columns `model_n` and `model_r`.
#'
#'   Rows with `SUB = "ALL"` cover all subsectors in a market. Rows with
#'   `GEO = "ALL"` are national totals: their `POP` is the summed population
#'   of the markets, and their density values are `NA` because they are not
#'   part of the cross-market regressions.
#'
#' @details A value of 0 means the market matches the expected density for
#'   its size; 0.5 means 50% more than expected; -0.5 means 50% fewer.
#'
#'   Every market in the population frame appears in every subsector, with
#'   `N = 0` where it has no organizations, so the regressions are not limited
#'   to markets that happen to have an organization in that subsector.
#'   Organizations are counted whether or not their resource value is
#'   missing; the resource total ignores missing values.
#'
#' @examples
#' toy <- data.frame(
#'   GEO = rep( c( "A", "B", "C", "D" ), times = c( 6, 3, 2, 1 ) ),
#'   SUB = c( "ART", "ART", "EDU", "EDU", "HEL", "HEL", "ART", "EDU", "HEL",
#'            "ART", "EDU", "ART" ),
#'   YEAR = 2020,
#'   TOTREV = c( 100, 50, 80, 20, 60, 40, 30, 20, 10, 15, 5, 8 ),
#'   POP = rep( c( 50000, 30000, 20000, 10000 ), times = c( 6, 3, 2, 1 ) ) )
#' get_scale_free_density( toy, geography = "GEO", population = "POP",
#'                         subsector = "SUB", year = "YEAR" )
#'
#' @export
get_scale_free_density <- function(df, geography = "CENSUS_CBSA_FIPS",
                                   population = "POP_METRO",
                                   subsector = "NTEEV2", year = "TAXYEAR",
                                   resource = "TOTREV", pop_table = NULL) {

  dat <- prep_market_data(df, geography, subsector, resource, year,
                          population, drop_missing_res = FALSE)

  # --- Market frame: one row per GEO x YEAR with population ---------------
  if (is.null(pop_table)) {
    frame <- dat[!is.na(dat$GEO) & !is.na(dat$POP), c("GEO", "YEAR", "POP")] |>
      dplyr::group_by(GEO, YEAR) |>
      dplyr::summarize(POP = mean(POP), .groups = "drop")
  } else {
    if (!all(c("GEO", "YEAR", "POP") %in% names(pop_table)))
      stop("pop_table must have columns GEO, YEAR and POP.", call. = FALSE)
    frame <- data.frame(GEO = as.character(pop_table$GEO),
                        YEAR = pop_table$YEAR,
                        POP = as.numeric(pop_table$POP))
    frame <- frame[!is.na(frame$POP), ]
    frame$YEAR <- if (is.numeric(dat$YEAR)) as.numeric(frame$YEAR) else as.character(frame$YEAR)
  }

  subs <- sort(unique(dat$SUB))

  # --- Counts and totals per market cell, zero-filled ----------------------
  count_cells <- function(d, keys) {
    d |>
      dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
      dplyr::summarize(N = dplyr::n(), RES2 = sum(RES, na.rm = TRUE),
                       .groups = "drop")
  }
  markets <- dat[!is.na(dat$GEO), ]

  cells_sub <- tidyr::expand_grid(frame, SUB = subs) |>
    dplyr::left_join(count_cells(markets, c("GEO", "SUB", "YEAR")),
                     by = c("GEO", "SUB", "YEAR"))
  cells_all <- frame |>
    dplyr::left_join(count_cells(markets, c("GEO", "YEAR")),
                     by = c("GEO", "YEAR")) |>
    dplyr::mutate(SUB = "ALL")

  cells <- dplyr::bind_rows(cells_sub, cells_all) |>
    dplyr::mutate(N = dplyr::coalesce(N, 0L),
                  RES2 = dplyr::coalesce(RES2, 0))

  # --- Fit log-log models across markets within SUB x YEAR -----------------
  cells <- cells |>
    dplyr::mutate(LOG_NPO = log(N + 1),
                  LOG_POP = log(POP + 1),
                  LOG_RES = log(RES2 + 1))

  fitted <- cells |>
    dplyr::group_by(SUB, YEAR) |>
    tidyr::nest() |>
    dplyr::mutate(
      model_n = purrr::map(data, \(d) fit_density_model(d, "LOG_NPO")),
      model_r = purrr::map(data, \(d) fit_density_model(d, "LOG_RES")),
      data = purrr::pmap(list(data, model_n, model_r), \(d, mn, mr)
        dplyr::mutate(d,
                      DENSITY_PCTL_N = density_pctl(mn, nrow(d)),
                      DENSITY_PCTL_R = density_pctl(mr, nrow(d))))
    ) |>
    tidyr::unnest(data) |>
    dplyr::ungroup()

  # --- National rows (GEO = "ALL"), not part of the regressions ------------
  national <- dat |>
    dplyr::group_by(SUB, YEAR) |>
    dplyr::summarize(N = dplyr::n(), RES2 = sum(RES, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::left_join(
      frame |> dplyr::group_by(YEAR) |>
        dplyr::summarize(POP = sum(POP), .groups = "drop"),
      by = "YEAR") |>
    dplyr::mutate(GEO = "ALL",
                  LOG_NPO = log(N + 1),
                  LOG_POP = log(POP + 1),
                  LOG_RES = log(RES2 + 1),
                  DENSITY_PCTL_N = NA_real_,
                  DENSITY_PCTL_R = NA_real_)

  log_resource <- paste0("LOG_", resource)

  dplyr::bind_rows(fitted, national) |>
    dplyr::select(GEO, SUB, YEAR, N, POP, RES2, LOG_NPO, LOG_POP, LOG_RES,
                  DENSITY_PCTL_N, DENSITY_PCTL_R, model_n, model_r) |>
    dplyr::arrange(GEO, SUB, YEAR) |>
    dplyr::rename({{resource}} := RES2, {{log_resource}} := LOG_RES)
}

# Fit log(outcome) ~ log(population); NULL when there are too few markets.
fit_density_model <- function(d, outcome) {
  if (nrow(d) < 3 || stats::var(d$LOG_POP) == 0) return(NULL)
  stats::lm(stats::reformulate("LOG_POP", outcome), data = d)
}

# Convert log-scale residuals to a proportional difference, capped at 2.
density_pctl <- function(model, n) {
  if (is.null(model)) return(rep(NA_real_, n))
  p <- exp(stats::residuals(model)) - 1
  p[p > 2] <- 2
  unname(p)
}
