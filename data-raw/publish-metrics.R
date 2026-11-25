###---------------------------------------------------
###   Build the published metric tables (GitHub release assets)
###---------------------------------------------------
#
# Builds national metric tables from live NCCS downloads and writes them,
# with a manifest and README, to a folder ready to attach to a GitHub
# release tagged "metrics-YYYY.MM". npc_load_metrics() reads them.
#
# Tables: one per universe x geography (x resource for filers), wide
# layout, as Parquet and gzipped CSV:
#   metrics_filers_<geo>_<RESOURCE>.{parquet,csv.gz}
#   metrics_registered_<geo>.{parquet,csv.gz}
#   metrics_grassroots_<geo>.{parquet,csv.gz}
#
# Run from the package root (downloads about 2 GB of NCCS files the first
# time; the build takes about an hour):
#   source( "data-raw/publish-metrics.R" )
# Then create the release, for example:
#   gh release create metrics-2026.11 data-local/published/metrics-2026.11/* \
#     --title "Metric tables 2026.11" --notes-file data-local/published/metrics-2026.11/README.md

devtools::load_all()

years     <- 2012:2022            # 2010-2011 use thinner legacy CORE files; 2023 is partial
geos      <- c( "cbsa", "county" )
resources <- c( "TOTREV", "TOTEXP", "NETASS" )
tag       <- Sys.getenv( "NPC_RELEASE_TAG", format( Sys.Date(), "metrics-%Y.%m" ) )
out       <- file.path( "data-local", "published", tag )
dir.create( out, recursive = TRUE, showWarnings = FALSE )

# Dynamics for the first year compare it with 2011, which comes from the
# thinner legacy CORE files, so they are left out.
dyn_cols <- c( "N_PREV", "ENTRIES", "EXITS", "EXITS_PPA", "ENTRY_RATE", "EXIT_RATE",
               "NET_GROWTH", "CHURN_RATE", "N_PREV_GRASSROOTS", "GRADUATIONS",
               "GRADUATION_RATE", "N_PREV_FILERS", "DOWNGRADES", "DOWNGRADE_RATE" )
blank_first_dynamics <- function( t ) {
  first <- t$YEAR == min( years )
  for ( v in intersect( dyn_cols, names( t ) ) ) t[[ v ]][ first ] <- NA
  t
}

write_table <- function( t, name ) {
  t <- blank_first_dynamics( as.data.frame( t ) )
  arrow::write_parquet( t, file.path( out, paste0( name, ".parquet" ) ) )
  data.table::fwrite( t, file.path( out, paste0( name, ".csv.gz" ) ) )
  data.frame( file = name, rows = nrow( t ), columns = ncol( t ),
              years = paste( range( t$YEAR ), collapse = "-" ) )
}

index <- list()
manifests <- list()
for ( u in c( "filers", "registered", "grassroots" ) ) {
  for ( g in geos ) {
    message( "== ", u, " / ", g, " ==" )
    res <- if ( u == "filers" ) resources else "TOTREV"
    w <- npc_build_metrics( years, geo = g, resources = res, universe = u,
                            source = "nccs", metrics = "all", format = "wide" )
    mf <- attr( w, "manifest" )
    for ( nm in names( w ) ) {
      r <- sub( "^[^_]*_?", "", sub( paste0( "^", g ), "", nm ) )
      file <- paste( c( "metrics", u, g, if ( nzchar( r ) ) r ), collapse = "_" )
      index[[ file ]] <- cbind( universe = u, geo = g, resource = r, write_table( w[[ nm ]], file ) )
    }
    mf$table <- paste( u, g, sep = "_" )
    manifests[[ length( manifests ) + 1 ]] <- mf
  }
}

index <- do.call( rbind, index )
utils::write.csv( index, file.path( out, "tables.csv" ), row.names = FALSE )
utils::write.csv( do.call( rbind, manifests ), file.path( out, "manifest.csv" ), row.names = FALSE )

writeLines( c(
  paste0( "# npcompete metric tables, ", tag ),
  "",
  "Precomputed competition, density and dynamics metrics for US nonprofit",
  "markets, built with npcompete from public NCCS data (CORE 990combined",
  "files and the unified geocoded BMF). Read them in R with",
  "`npcompete::npc_load_metrics()`.",
  "",
  paste0( "- Tax years: ", min( years ), "-", max( years ) ),
  "- Organizations: 501(c)(3)",
  "- Universes: filers (Form 990 / 990-EZ), registered (every organization on the IRS registry), grassroots (990-N filers and non-filers)",
  "- Geographies: CBSAs (2023 delineation) and counties (2020 boundaries)",
  "- Resources (filers): total revenue, total expenses, net assets",
  "- Metrics: every metric in `npc_metrics()`, including dynamics (entry, exit, churn; graduation and downgrade for registered)",
  "",
  "Each table has one row per market (`GEO`), subsector (`SUB`, NTEEV2 major group or `ALL`) and year;",
  "`GEO = \"ALL\"` rows are national. `tables.csv` lists the files; `manifest.csv` records how each was built.",
  "",
  "Caveats: dynamics start in 2013 (2012 would compare with 2011, from thinner legacy CORE files);",
  "dynamics need the following year to confirm exits, so 2022 exits for filers are `NA`;",
  "registered exits in 2017-2018 are distorted by a gap in NCCS's BMF releases (pool 2016-2019).",
  "See https://nonprofit-open-data-collective.github.io/npcompete/articles/dynamics.html.",
  "",
  paste0( "Built ", format( Sys.time(), "%Y-%m-%d" ), " with npcompete ",
          as.character( utils::packageVersion( "npcompete" ) ), "." ) ),
  file.path( out, "README.md" ) )

message( "Wrote ", nrow( index ), " tables to ", out )
