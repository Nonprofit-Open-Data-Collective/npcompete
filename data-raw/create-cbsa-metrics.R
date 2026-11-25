###---------------------------------------------------
###   Build the cbsa_metrics dataset
###---------------------------------------------------
#
# Every competition metric for every metro and micro area (CBSA) in the US,
# by NTEE major group, for tax year 2019, measured on total revenue. Built
# with get_all_metrics() from the SOI panel sample frame, so it matches the
# current package code. The vignettes use it for cross-market plots, which
# the Vermont-only npo_panel_sample is too small for.

library( data.table )
devtools::load_all()

archive_dir <- Sys.getenv( "NPCOMPETE_ARCHIVE", "data-local" )
fn <- file.path( archive_dir, "SOI-PANEL-SAMPLE-FRAME-COLUMNS-AUGMENTED-W-POP.CSV" )

keep <- c( "EIN2", "TAXYEAR", "CENSUS_CBSA_FIPS", "V501C", "NTEEV2",
           "TOTREV", "POP_METRO" )

d <- fread( fn, select = keep,
            colClasses = list( character = "CENSUS_CBSA_FIPS" ) )
d <- d[ TAXYEAR == 2019 & V501C == 3 ]
d[ CENSUS_CBSA_FIPS == "", CENSUS_CBSA_FIPS := NA ]

m <- get_all_metrics( as.data.frame( d ),
                      geography  = "CENSUS_CBSA_FIPS",
                      population = "POP_METRO",
                      resource   = "TOTREV" )

# expected values from the density models, which get_all_metrics() drops
den <- get_scale_free_density( as.data.frame( d ),
                               geography  = "CENSUS_CBSA_FIPS",
                               population = "POP_METRO",
                               resource   = "TOTREV" )
den <- den[ , c( "GEO", "SUB", "YEAR", "EXPECTED_N", "EXPECTED_TOTREV" ) ]
m   <- dplyr::left_join( m, den, by = c( "GEO", "SUB", "YEAR" ) )

# market rows only: drop the national (GEO = "ALL") rollups
cbsa_metrics <- as.data.frame( m[ m$GEO != "ALL", ] )
cbsa_metrics <- subset( cbsa_metrics, select = -c( LOG_NPO, LOG_POP, LOG_TOTREV ) )
cbsa_metrics <- dplyr::relocate( cbsa_metrics, EXPECTED_N, EXPECTED_TOTREV,
                                 .after = TOTREV )

usethis::use_data( cbsa_metrics, overwrite = TRUE, compress = "xz" )
