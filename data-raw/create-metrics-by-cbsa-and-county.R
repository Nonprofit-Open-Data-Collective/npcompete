###---------------------------------------------------
###   Build concentration metrics by CBSA and county
###---------------------------------------------------
#
# Input: the SOI panel sample frame with population merged on, produced by
# data-raw/add-pop-soi-sample-frame-panel.R. It is 2+ GB and is not stored in
# the repo; point `archive_dir` at the folder that holds it.

library( data.table )
devtools::load_all()

archive_dir <- Sys.getenv( "NPCOMPETE_ARCHIVE", "data-local" )
fn <- file.path( archive_dir, "SOI-PANEL-SAMPLE-FRAME-COLUMNS-AUGMENTED-W-POP.CSV" )

df1 <- fread( fn )
df  <- dplyr::filter( df1, TAXYEAR > 2010 & TAXYEAR < 2023 )   # TY2023+ incomplete
df  <- dplyr::filter( df, V501C == 3 )

# Example output (2011-2016, county 01001, all subsectors):
#  GEO    SUB  YEAR  N   POP    TOTREV    ...  GINI   HHI     NHHI    KWOKA  CR1
#  01001  ALL  2011  38  54994  15964703  ...  0.652  0.0998  0.0754  0.363  0.229


####
####   METROS
####

for( res in c( "TOTREV", "TOTEXP", "NETASS" ) )
{
  m <- get_all_metrics( df, resource = res,
                        geography = "CENSUS_CBSA_FIPS",
                        population = "POP_METRO" )
  fwrite( m, file.path( archive_dir, paste0( "CONCENTRATION-METRICS-BY-CBSA-", res, ".CSV" ) ) )
}


####
####   COUNTIES
####

for( res in c( "TOTREV", "TOTEXP", "NETASS" ) )
{
  m <- get_all_metrics( df, resource = res,
                        geography = "COUNTY_FIPS",
                        population = "POP_COUNTY" )
  fwrite( m, file.path( archive_dir, paste0( "CONCENTRATION-METRICS-BY-COUNTY-", res, ".CSV" ) ) )
}


# Check: no duplicate GEO-SUB-YEAR rows after the joins
m |>
  dplyr::count( GEO, SUB, YEAR ) |>
  dplyr::filter( n > 1 )
