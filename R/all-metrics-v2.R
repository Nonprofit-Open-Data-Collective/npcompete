#' Compute all market concentration metrics and return a single data frame
#'
#' Combines Gini, normalized HHI, Kwoka, and concentration ratios (CR1–CR8)
#' into a unified summary for each (GEO, SUB, YEAR) group.
#'
#' @param df Data frame containing market data.
#' @param geography Column name identifying the geography field.
#' @param subsector Column name identifying the subsector field.
#' @param resource Column name identifying the resource or revenue variable.
#' @param year Column name identifying the year field.
#' @param numbers Numeric vector of CR levels to compute (default = 1:8).
#'
#' @return A merged tibble with columns:
#'   GEO, SUB, YEAR, N, TOTREV, GINI, HHI, NHHI, KWOKA, CR1–CR8
#'
#' @export
get_all_metrics <- function(df,
                                  geography = "CENSUS_CBSA_FIPS",
                                  population = "POP_METRO",
                                  subsector = "NTEEV2",
                                  resource  = "TOTREV",
                                  year      = "TAXYEAR",
                                  cr_nums   = 1:8) {

  # --- Run all individual metrics ------------------------------------------
  message("Calculating Gini...")
  dat_gini  <- get_gini(df, geography, subsector, resource, year)

  message("Calculating normalized HHI...")
  dat_nhhi  <- get_nhhi(df, geography, subsector, resource, year)

  message("Calculating Kwoka index...")
  dat_kwoka <- get_kwoka(df, geography, subsector, resource, year)

  message("Calculating concentration ratios...")
  dat_cr    <- get_cr_all(df, geography, subsector, year, resource, numbers=cr_nums)

  message("Calculating nonprofit density...")
  dat_den   <- get_scale_free_density(df, geography, population, subsector, year, resource)
  dat_den   <- dplyr::select( dat_den, -model_n, -model_r, -N, -{{resource}} ) 

  # --- Standardize join keys ----------------------------------------------
  by_keys <- c("GEO", "SUB", "YEAR")

  # --- Merge all results ---------------------------------------------------
  out <- dat_gini %>%
    dplyr::full_join( dat_nhhi ) %>%
    dplyr::full_join( dat_kwoka ) %>%
    dplyr::full_join( dat_cr ) %>%
    dplyr::full_join( dat_den, by = by_keys ) 

  # --- Tidy up column order ------------------------------------------------
  log_resource <- paste0( "LOG_", resource )
  metric_cols <- c(
    "N", "POP", resource, "LOG_NPO", "LOG_POP", log_resource, 
    "DENSITY_PCTL_N", "DENSITY_PCTL_R", "GINI", "HHI", "NHHI", "KWOKA",
    paste0("CR", numbers)
  )
  existing_cols <- intersect(metric_cols, names(out))
  out <- dplyr::select(out, GEO, SUB, YEAR, dplyr::all_of(existing_cols))

  return(out)
}

# all_m_totrev <- get_all_metrics( df, resource  = "TOTREV" )
# all_m_totexp <- get_all_metrics( df, resource  = "TOTEXP" )
# all_m_netass <- get_all_metrics( df, resource  = "NETASS" )


all_m_totrev %>% 
  arrange( GEO, YEAR, SUB ) %>%
  group_by( GEO, SUB, YEAR ) %>% 
  mutate( count=n() ) %>% 
  dplyr::filter( count > 1 )


