###---------------------------------------------------
###   Summary data for vignettes/dynamics.Rmd
###---------------------------------------------------
#
# Builds entry, exit and transition rates from live NCCS downloads and saves
# the small summary tables the dynamics article plots, so the article runs
# without downloading anything. About 1.5 GB of NCCS files on first run.
#
# Run from the package root:
#   source( "data-raw/create-dynamics-vignette-data.R" )

devtools::load_all()

dyn <- c( "ENTRY_RATE", "EXIT_RATE", "NET_GROWTH", "CHURN_RATE" )
grab <- function( t, cols ) as.data.frame( t[ , intersect( cols, names( t ) ) ] )

# Filers: tax years 2013-2021 (2011-2012 and 2022 are read for gaps and exits)
filers <- npc_build_metrics( 2013:2021, geo = "cbsa", source = "nccs",
                             metrics = dyn, format = "wide" )$cbsa

# Registered universe: 2011-2022, with and without PPA revocations
reg_ids <- c( dyn, "GRADUATION_RATE", "DOWNGRADE_RATE" )
registered <- npc_build_metrics( 2011:2022, geo = "cbsa", source = "nccs",
                                 universe = "registered", metrics = reg_ids,
                                 format = "wide" )$cbsa
reg_noppa <- npc_build_metrics( 2011:2022, geo = "cbsa", source = "nccs",
                                universe = "registered", metrics = dyn,
                                drop_ppa_revocations = TRUE, format = "wide" )$cbsa

keep <- c( "GEO", "SUB", "YEAR", "N", "POP", "N_PREV", "ENTRIES", "EXITS",
           "EXITS_PPA", dyn, "N_PREV_GRASSROOTS", "GRADUATIONS", "GRADUATION_RATE",
           "N_PREV_FILERS", "DOWNGRADES", "DOWNGRADE_RATE" )

nat <- function( t, sub = "ALL" ) grab( t[ t$GEO == "ALL" & t$SUB %in% sub, ], keep )

dynamics_data <- list(
  filers_national     = nat( filers ),
  filers_subsector    = nat( filers, setdiff( unique( filers$SUB ), "ALL" ) ),
  filers_cbsa         = grab( filers[ filers$GEO != "ALL" & filers$SUB == "ALL", ], keep ),
  registered_national = nat( registered ),
  registered_subsector = nat( registered, setdiff( unique( registered$SUB ), "ALL" ) ),
  registered_noppa    = nat( reg_noppa ),
  registered_counts   = utils::read.csv( file.path( npc_cache_dir(), "registered-nccs",
                                                    "year-counts.csv" ) ),
  built = format( Sys.time(), "%Y-%m-%d" ),
  npcompete_version = as.character( utils::packageVersion( "npcompete" ) ) )

saveRDS( dynamics_data, "vignettes/dynamics-data.rds", compress = "xz" )
