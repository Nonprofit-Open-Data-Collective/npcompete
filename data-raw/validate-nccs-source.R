###---------------------------------------------------
###   Validate the live NCCS source against the SOI panel archive
###---------------------------------------------------
#
# Builds tax years from NCCS downloads (npc_build_nccs) and compares them
# with the same years in the local SOI panel archive: counts and resource
# totals by year, an EIN-level match of values and county, and counts by
# market.
#
# Run from the package root. Set NPCOMPETE_ARCHIVE to the archive and,
# optionally, NPCOMPETE_CACHE to where the NCCS downloads should go
# (about 0.7 GB for the BMF plus about 50 MB per CORE file).

devtools::load_all()

arch  <- Sys.getenv( "NPCOMPETE_ARCHIVE", "data-local/archive" )
cache <- npc_cache_dir()
years <- as.integer( strsplit( Sys.getenv( "NPC_VALIDATE_YEARS", "2018,2019" ), "," )[[ 1 ]] )

npc_build_nccs( years, dir = cache )

for ( g in c( "county", "cbsa" ) )
{
  cmp <- npc_compare_sources( years, archive_dir = arch, nccs_dir = cache, geo = g )
  if ( g == "county" )
  {
    cat( "\n== Totals by year and source ==\n" );  print( cmp$totals )
    cat( "\n== EIN-level match ==\n" );            print( cmp$ein_match )
  }
  cat( "\n== Market counts:", g, "(markets with 10+ archive orgs) ==\n" )
  print( cmp$market_summary )
}
