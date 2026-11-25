###---------------------------------------------------
###   Validate the archive build against the original CSVs
###---------------------------------------------------
#
# Rebuilds the CBSA concentration tables from the Parquet archive with
# npc_build_metrics() and compares them with the CONCENTRATION-METRICS-*.CSV
# files produced by the original script (data-raw/create-metrics-by-cbsa-
# and-county.R, pre-Phase 1 code).
#
# Expected differences, by design:
#   - KWOKA: standard dominance formula (Phase 1)
#   - NHHI: 1 instead of 0 for single-org markets; NA for all-zero markets
#   - density: zero-count cells, market-level population, new model
#   - rows with a blank CBSA are no longer reported as a market
#
# Run from the package root with NPCOMPETE_ARCHIVE pointing at the archive.

library( dplyr )
devtools::load_all()

arch <- Sys.getenv( "NPCOMPETE_ARCHIVE", "data-local/archive" )
old_dir <- "data-local"

new <- npc_build_metrics( 2011:2022, geo = "cbsa",
                          resources = c( "TOTREV", "TOTEXP", "NETASS" ),
                          cbsa = "geocode", density_model = "loglinear",
                          dir = arch, format = "wide" )

compare_one <- function( res )
{
  old <- data.table::fread( file.path( old_dir, paste0( "CONCENTRATION-METRICS-", res, ".CSV" ) ),
                            colClasses = list( character = c( "GEO", "SUB" ) ) ) |>
    as.data.frame() |>
    filter( YEAR %in% 2011:2022 )
  nw <- new[[ paste0( "cbsa_", res ) ]] |> as.data.frame()

  key <- c( "GEO", "SUB", "YEAR" )
  m <- inner_join( old, nw, by = key, suffix = c( ".old", ".new" ) ) |>
    filter( GEO != "ALL", GEO != "" )

  same <- function( v, tol = 1e-8 ) {
    x <- m[[ paste0( v, ".old" ) ]]; y <- m[[ paste0( v, ".new" ) ]]
    ok <- ( is.na( x ) & is.na( y ) ) | ( !is.na( x ) & !is.na( y ) & abs( x - y ) <= tol * pmax( 1, abs( x ) ) )
    c( differ = sum( !ok ), of = length( ok ) )
  }
  vars <- c( "N", res, "GINI", "HHI", paste0( "CR", 1:8 ), "NHHI", "KWOKA", "DENSITY_PCTL_N" )
  tab <- t( sapply( vars, same ) )
  single <- m$N.old == 1
  nhhi_other <- sum( abs( m$NHHI.old - m$NHHI.new )[ !single ] > 1e-8, na.rm = TRUE )

  list(
    resource = res,
    rows_old = nrow( old ), rows_new = nrow( nw ),
    old_only = nrow( anti_join( old, nw, by = key ) ),
    old_only_blank_geo = sum( anti_join( old, nw, by = key )$GEO == "" ),
    new_only = nrow( anti_join( nw, old, by = key ) ),
    new_only_zero_n = sum( anti_join( nw, old, by = key )$N == 0 ),
    matched_market_rows = nrow( m ),
    by_metric = tab,
    nhhi_diffs_excluding_single_org = nhhi_other
  )
}

results <- lapply( c( "TOTREV", "TOTEXP", "NETASS" ), compare_one )
print( results )
