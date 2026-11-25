###---------------------------------------------------
###   Scale-free density
###---------------------------------------------------

#' Scale-free nonprofit density
#'
#' Measures whether a market has more or fewer nonprofits (and more or less of
#' a resource) than expected for its population. Within each subsector and
#' year, the number of nonprofits and the resource total are modeled as a
#' power function of population across markets, and each market's observed
#' value is compared with the value expected for its size.
#'
#' @inheritParams npcompete-params
#' @param pop_table Optional data frame of market populations with columns
#'   `GEO`, `YEAR` and `POP`. Supply it so that markets with no organizations
#'   at all are still included. When `NULL`, market populations are taken from
#'   the `population` column of `df`, so only markets with at least one
#'   organization (in any subsector) are included.
#' @param model How the expected values are estimated:
#'   * `"negbin"` (default): negative binomial regression of the count on
#'     log population, which handles zero counts and overdispersion. Falls
#'     back to Poisson when the negative binomial fit fails.
#'   * `"poisson"`: Poisson regression of the count on log population.
#'   * `"loglinear"`: least squares regression of `log(N + 1)` on
#'     `log(POP + 1)`, the original method.
#'
#'   For the resource total, both count options use Poisson pseudo-maximum
#'   likelihood (a quasi-Poisson GLM), which is suited to non-negative dollar
#'   amounts with many zeros.
#'
#' @return A tibble with one row per `GEO`, `SUB` and `YEAR`, containing `N`,
#'   `POP`, the summed resource, `LOG_NPO`, `LOG_POP`, `LOG_<resource>`,
#'   `EXPECTED_N`, `EXPECTED_<resource>`, `DENSITY_PCTL_N`, `DENSITY_PCTL_R`
#'   (both capped at 2) and the fitted models in the list columns `model_n`
#'   and `model_r`. The model type is stored in the `"density_model"`
#'   attribute.
#'
#'   Rows with `SUB = "ALL"` cover all subsectors in a market. Rows with
#'   `GEO = "ALL"` are national totals: their `POP` is the summed population
#'   of the markets, and their density values are `NA` because they are not
#'   part of the cross-market models.
#'
#' @details The density index is `observed / expected - 1`: 0 means the
#'   market matches the expected density for its size, 0.5 means 50% more
#'   than expected, and -1 means none at all. The `"loglinear"` model computes
#'   it as `exp(residual) - 1` on the `log(x + 1)` scale, which compresses
#'   differences among small markets.
#'
#'   Every market in the population frame appears in every subsector, with
#'   `N = 0` where it has no organizations, so the models are not limited
#'   to markets that happen to have an organization in that subsector.
#'   Organizations are counted whether or not their resource value is
#'   missing; the resource total ignores missing values.
#'
#' @examples
#' d <- get_scale_free_density( npo_panel_sample,
#'                              geography = "COUNTY_FIPS",
#'                              population = "POP_COUNTY" )
#' head( d[ d$SUB == "ALL", c( "GEO", "YEAR", "N", "EXPECTED_N", "DENSITY_PCTL_N" ) ] )
#'
#' @export
get_scale_free_density <- function(df, geography = "CENSUS_CBSA_FIPS",
                                   population = "POP_METRO",
                                   subsector = "NTEEV2", year = "TAXYEAR",
                                   resource = "TOTREV", pop_table = NULL,
                                   model = c("negbin", "poisson", "loglinear")) {

  model <- match.arg(model)
  if (is.null(pop_table) && is.null(population))
    stop("Supply either a `population` column or a `pop_table`.", call. = FALSE)
  if (!is.null(pop_table)) population <- NULL       # market-level population only

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
                  RES2 = dplyr::coalesce(RES2, 0),
                  LOG_NPO = log(N + 1),
                  LOG_POP = log(POP + 1),
                  LOG_RES = log(RES2 + 1))

  # --- Fit models across markets within SUB x YEAR -------------------------
  fitted <- cells |>
    dplyr::group_by(SUB, YEAR) |>
    tidyr::nest() |>
    dplyr::mutate(
      model_n = purrr::map(data, \(d) fit_density_model(d, "N", model)),
      model_r = purrr::map(data, \(d) fit_density_model(d, "RES2", model)),
      data = purrr::pmap(list(data, model_n, model_r), \(d, mn, mr) {
        en <- expected_value(mn, nrow(d))
        er <- expected_value(mr, nrow(d))
        dplyr::mutate(d,
                      EXPECTED_N = en,
                      EXPECTED_RES = er,
                      DENSITY_PCTL_N = density_index(mn, d$N, en),
                      DENSITY_PCTL_R = density_index(mr, d$RES2, er))
      })
    ) |>
    tidyr::unnest(data) |>
    dplyr::ungroup()

  # --- National rows (GEO = "ALL"), not part of the models -----------------
  national <- dplyr::bind_rows(
      count_cells(dat, c("SUB", "YEAR")),
      count_cells(dat, "YEAR") |> dplyr::mutate(SUB = "ALL")) |>
    dplyr::left_join(
      frame |> dplyr::group_by(YEAR) |>
        dplyr::summarize(POP = sum(POP), .groups = "drop"),
      by = "YEAR") |>
    dplyr::mutate(GEO = "ALL",
                  LOG_NPO = log(N + 1),
                  LOG_POP = log(POP + 1),
                  LOG_RES = log(RES2 + 1),
                  EXPECTED_N = NA_real_,
                  EXPECTED_RES = NA_real_,
                  DENSITY_PCTL_N = NA_real_,
                  DENSITY_PCTL_R = NA_real_)

  log_resource <- paste0("LOG_", resource)
  expected_resource <- paste0("EXPECTED_", resource)

  out <- dplyr::bind_rows(fitted, national) |>
    dplyr::select(GEO, SUB, YEAR, N, POP, RES2, LOG_NPO, LOG_POP, LOG_RES,
                  EXPECTED_N, EXPECTED_RES, DENSITY_PCTL_N, DENSITY_PCTL_R,
                  model_n, model_r) |>
    dplyr::arrange(GEO, SUB, YEAR) |>
    dplyr::rename({{resource}} := RES2, {{log_resource}} := LOG_RES,
                  {{expected_resource}} := EXPECTED_RES)
  attr(out, "density_model") <- model
  out
}

# Fit the density model for one SUB x YEAR group. `outcome` is "N" (count)
# or "RES2" (resource total). Returns NULL when there are too few markets
# or no variation to fit.
fit_density_model <- function(d, outcome, model) {
  y <- d[[outcome]]
  if (nrow(d) < 3 || stats::var(d$LOG_POP) == 0 || all(y == 0)) return(NULL)

  if (model == "loglinear") {
    d$Y <- log(y + 1)
    return(stats::lm(Y ~ LOG_POP, data = d))
  }

  ctrl <- stats::glm.control(maxit = 100)
  fit_glm <- function(family) {
    fit <- tryCatch(suppressWarnings(
      stats::glm(Y ~ LOG_POP, family = family, data = d, control = ctrl)),
      error = function(e) NULL)
    if (is.null(fit) || !fit$converged) NULL else fit
  }

  if (outcome == "RES2") {
    # Poisson pseudo-maximum likelihood for non-negative dollar totals. The
    # outcome is divided by its mean for numerical stability; this changes
    # only the intercept, and fitted values are scaled back.
    s <- mean(y)
    d$Y <- y / s
    fit <- fit_glm(stats::quasipoisson())
    if (!is.null(fit)) fit$npc_scale <- s
    return(fit)
  }

  d$Y <- y
  if (model == "negbin") {
    fit <- tryCatch(
      suppressWarnings(MASS::glm.nb(Y ~ LOG_POP, data = d, control = ctrl)),
      error = function(e) NULL)
    if (!is.null(fit) && fit$converged && is.finite(fit$theta)) return(fit)
  }
  fit_glm(stats::poisson())
}

# Expected values on the original scale (counts or dollars).
expected_value <- function(m, n) {
  if (is.null(m)) return(rep(NA_real_, n))
  if (inherits(m, "glm")) {
    s <- if (is.null(m$npc_scale)) 1 else m$npc_scale
    return(unname(stats::fitted(m)) * s)
  }
  unname(exp(stats::fitted(m)) - 1)                 # loglinear: back-transform
}

# Density index: observed / expected - 1, capped at 2. The loglinear model
# keeps its original definition, exp(residual) - 1 on the log(x + 1) scale.
density_index <- function(m, observed, expected) {
  if (is.null(m)) return(rep(NA_real_, length(observed)))
  p <- if (inherits(m, "glm")) observed / expected - 1
       else exp(stats::residuals(m)) - 1
  p <- unname(p)
  p[p > 2] <- 2
  p
}
