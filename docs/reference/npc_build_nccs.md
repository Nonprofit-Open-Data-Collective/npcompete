# Build the harmonized organization table from NCCS downloads

Downloads the NCCS CORE 990combined files and the unified geocoded BMF
for the requested tax years, harmonizes them to the canonical schema
with
[`npc_standardize()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_standardize.md),
and writes them to `dir/filers/` as a Parquet dataset partitioned by
`YEAR`, the same layout as
[`npc_build_archive()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_build_archive.md).
Years already built are kept, so the table can be grown a few years at a
time.
[`npc_get_orgs()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_get_orgs.md)
and
[`npc_build_metrics()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_build_metrics.md)
with `source = "nccs"` call this for any year they need that is not
built yet.

## Usage

``` r
npc_build_nccs(years, dir = npc_cache_dir(), refresh = FALSE, keep_raw = TRUE)
```

## Arguments

- years:

  Integer vector of tax years. NCCS publishes 990combined files from
  2010, so the earliest full tax year is 2010.

- dir:

  Cache directory; see
  [`npc_cache_dir()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_cache_dir.md).
  Any folder works, so a live build can be kept as a frozen archive.

- refresh:

  Download the NCCS files again and rebuild the requested years even if
  they are cached.

- keep_raw:

  Keep the downloaded CORE files after the build. The BMF is always
  kept.

## Value

The cache directory, invisibly. It holds `filers/`, `year-counts.csv`
(rows per tax year, with the files and BMF used), `manifest.csv` and
`downloads.csv` (URL, size and MD5 of every file).

## Details

Tax year `t` combines December year ends from CORE file `t` with other
year ends from file `t + 1`, so it needs both files. A tax year whose
`t + 1` file holds less than 80% of the rows of file `t` is flagged
incomplete (see
[`npc_archive_info()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_archive_info.md)).

Location and subsector come from the unified BMF, one record per EIN, so
an organization that moved is placed at its latest address in every
year. The county is found by matching the geocoded county and state
names to
[pop_county](https://nonprofit-open-data-collective.github.io/npcompete/reference/pop_county.md)
(see
[`npc_match_county()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_match_county.md)).

## Examples

``` r
if (FALSE) { # \dontrun{
npc_build_nccs( 2018:2020 )
npc_archive_info( npc_cache_dir() )
} # }
```
