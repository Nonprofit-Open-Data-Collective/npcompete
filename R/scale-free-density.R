###---------------------------------------------------
###   Scale-free density
###---------------------------------------------------

#' Scale-free nonprofit density
#'
#' Measures whether a market has more or fewer nonprofits (and more or less of
#' a resource) than expected for its population. Within each subsector and
#' year, the logged count of nonprofits and the logged resource total are
#' regressed on logged population. The residuals are converted back to a
#' proportional difference from the expected value.
#'
#' @inheritParams npcompete-params
#'
#' @return A tibble with one row per `GEO`, `SUB` and `YEAR` (plus `ALL`
#'   rollups) containing `N`, `POP`, the summed resource, the logged values,
#'   `DENSITY_PCTL_N` and `DENSITY_PCTL_R` (both capped at 2), and the fitted
#'   models in the list columns `model_n` and `model_r`.
#'
#' @details A value of 0 means the market matches the expected density for
#'   its size; 0.5 means 50% more than expected; -0.5 means 50% fewer.
#'
#'   Known issues, scheduled for Phase 1 of `DEV-PLAN.md`: markets with no
#'   organizations in a subsector are left out of the regressions, and the
#'   national `GEO = "ALL"` rows are fitted along with the markets.
#'
#' @examples
#' \dontrun{
#' get_scale_free_density( df, geography = "CENSUS_CBSA_FIPS",
#'                         population = "POP_METRO", resource = "TOTREV" )
#' }
#'
#' @export
get_scale_free_density <- function(df, geography="CENSUS_CBSA_FIPS", population="POP_METRO", subsector="NTEEV2", year="TAXYEAR", resource="TOTREV"){

  GEO <- df[[geography]] |> factor( exclude = NULL )
  if( subsector == "NTEEV2" )
  { SUB <- df[[subsector]] |> substr(1,3) |> factor( exclude = NULL ) } else
  { SUB <- df[[subsector]] |> factor( exclude = NULL ) }
  POP <- df[[population]]  |> as.numeric()
  RES <- df[[resource]] |> as.numeric()
  RES <- bottomcode(RES)
  YEAR <- df[[year]] |> factor()

  log_resource <- paste0( "LOG_", resource )

    dd1 <-
      data.frame(GEO,SUB,YEAR,POP,RES) %>%
      stats::na.omit() %>%
      dplyr::group_by( GEO, SUB, YEAR ) %>%
      dplyr::summarize( N=dplyr::n(),
                        POP2 = mean( POP, na.rm=TRUE ),
                        RES2 = sum( RES, na.rm=TRUE ),
                        LOG_NPO = log( N + 1 ),
                        LOG_POP = log( POP2 + 1 ),
                        LOG_RES = log( RES2 + 1 ),
                        .groups = "drop" )

    dd2 <-
      data.frame(GEO,YEAR,POP,RES) %>%
      stats::na.omit() %>%
      dplyr::group_by( GEO, YEAR ) %>%
      dplyr::summarize( N=dplyr::n(),
                        POP2 = mean(POP, na.rm=TRUE),
                        RES2 = sum(RES, na.rm=TRUE),
                        LOG_NPO = log( N + 1 ),
                        LOG_POP = log( POP2 + 1 ),
                        LOG_RES = log( RES2 + 1 ),
                        .groups = "drop" )
    dd2$SUB <- "ALL"

    dd3 <-
      data.frame(SUB,YEAR,POP,RES) %>%
      stats::na.omit() %>%
      dplyr::group_by( SUB, YEAR ) %>%
      dplyr::summarize( N=dplyr::n(),
                        POP2 = mean(POP, na.rm=TRUE),
                        RES2 = sum(RES, na.rm=TRUE),
                        LOG_NPO = log( N + 1 ),
                        LOG_POP = log( POP2 + 1 ),
                        LOG_RES = log( RES2 + 1 ),
                        .groups = "drop" )
    dd3$GEO <- "ALL"

    dd <- dplyr::bind_rows( dd1, dd2, dd3 )
    dd <- dplyr::arrange( dd, GEO, SUB, YEAR )

  get_density_pctl_n <- function(df) {
    m <- stats::lm(LOG_NPO ~ LOG_POP, data = df)
    logged_residuals <- stats::residuals(m)
    DENSITY_PCTL_N <- exp(logged_residuals) - 1
    DENSITY_PCTL_N[DENSITY_PCTL_N > 2] <- 2
    return(list(model_n = m, DENSITY_PCTL_N = DENSITY_PCTL_N))
  }

  get_density_pctl_r <- function(df) {
    m <- stats::lm(LOG_RES ~ LOG_POP, data = df)
    logged_residuals <- stats::residuals(m)
    DENSITY_PCTL_R <- exp(logged_residuals) - 1
    DENSITY_PCTL_R[DENSITY_PCTL_R > 2] <- 2
    return(list(model_r = m, DENSITY_PCTL_R = DENSITY_PCTL_R))
  }

  df_density <- dd %>%
    dplyr::group_by(SUB, YEAR) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      model_results_n = purrr::map(data, get_density_pctl_n),
      model_n = purrr::map(model_results_n, "model_n"),
      data = purrr::map2(data, model_results_n, \(d, res) dplyr::mutate(d, DENSITY_PCTL_N = res$DENSITY_PCTL_N))
    ) %>%
    dplyr::mutate(
      model_results_r = purrr::map(data, get_density_pctl_r),
      model_r = purrr::map(model_results_r, "model_r"),
      data = purrr::map2(data, model_results_r, \(d, res) dplyr::mutate(d, DENSITY_PCTL_R = res$DENSITY_PCTL_R))
    ) %>%
    dplyr::select(-model_results_n,-model_results_r) %>%
    tidyr::unnest(data) %>%
    dplyr::rename( POP=POP2, {{resource}} := RES2, {{log_resource}} := LOG_RES )

  return( df_density )
}
