# Compare live NCCS filers with the local archive

Reads the same tax years from the SOI panel archive and from a live NCCS
build and reports how far they agree: organization counts and resource
totals by year, counts by market, and an EIN-level match of financial
values and county. Imputed gap-year rows are left out of the archive
side, since live builds have none.

## Usage

``` r
npc_compare_sources(
  years,
  archive_dir = npc_archive_dir(),
  nccs_dir = npc_cache_dir(),
  geo = c("county", "cbsa"),
  resources = c("TOTREV", "TOTEXP", "NETASS", "TOTASS"),
  subsection = 3
)
```

## Arguments

- years:

  Tax years present in both sources.

- archive_dir:

  Archive directory; see
  [`npc_archive_dir()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_archive_dir.md).

- nccs_dir:

  Live build directory; see
  [`npc_cache_dir()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_cache_dir.md).
  Missing years are built.

- geo:

  Market level for the by-market comparison: `"county"` or `"cbsa"`.

- resources:

  Resource columns to compare.

- subsection:

  501(c) subsections to keep, or `NULL` for all.

## Value

A list of data frames:

- totals:

  By year and source: rows, distinct organizations and resource totals

- ein_match:

  By year: EINs in both sources or only one, extra archive rows for an
  EIN already seen that year, and among EINs in both, the share with the
  same filer type, county, and each resource within \$1

- markets:

  By market and year: organizations in each source, the difference and
  the ratio

- market_summary:

  By year: correlation of market counts and the median absolute percent
  difference, for markets with 10 or more archive organizations

## Examples

``` r
if (FALSE) { # \dontrun{
cmp <- npc_compare_sources( 2017:2019 )
cmp$totals
cmp$ein_match
} # }
```
