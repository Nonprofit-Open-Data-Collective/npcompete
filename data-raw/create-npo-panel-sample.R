###---------------------------------------------------
###   Build the npo_panel_sample dataset
###---------------------------------------------------
#
# A small extract of the SOI panel sample frame in the current schema:
# every 501(c)(3) filer in Vermont (state FIPS 50), tax years 2015-2019.
# Keeping whole counties and metros intact makes the extract usable for the
# density models, which need every market in the frame.

library( data.table )

archive_dir <- Sys.getenv( "NPCOMPETE_ARCHIVE", "data-local" )
fn <- file.path( archive_dir, "SOI-PANEL-SAMPLE-FRAME-COLUMNS-AUGMENTED-W-POP.CSV" )

keep <- c( "EIN2", "TAXYEAR", "COUNTY_FIPS", "CENSUS_CBSA_FIPS", "V501C",
           "NTEEV2", "TOTREV", "TOTEXP", "NETASS", "TOTASS",
           "REVPROG", "REVCONT", "FNDFEE", "ORG_RULING_YEAR",
           "REVOKED_YEAR_X", "MISSING_YEAR", "POP_COUNTY", "POP_METRO" )

d <- fread( fn, select = keep,
            colClasses = list( character = c( "COUNTY_FIPS", "CENSUS_CBSA_FIPS" ) ) )

d[ , COUNTY_FIPS := sprintf( "%05d", as.integer( COUNTY_FIPS ) ) ]
npo_panel_sample <- d[ substr( COUNTY_FIPS, 1, 2 ) == "50" &
                       TAXYEAR %in% 2015:2019 &
                       V501C == 3 ]
npo_panel_sample[ CENSUS_CBSA_FIPS == "" , CENSUS_CBSA_FIPS := NA ]
setorder( npo_panel_sample, EIN2, TAXYEAR )
npo_panel_sample <- as.data.frame( npo_panel_sample )

usethis::use_data( npo_panel_sample, overwrite = TRUE, compress = "xz" )
