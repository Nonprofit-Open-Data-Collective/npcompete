# Build the registered universe of organizations

Builds one row per organization and tax year for every organization on
the IRS registry, filers and non-filers alike, and classifies each
organization-year by what it filed. The result is written to
`dir/registered-<filer_source>/orgs/` as a Parquet dataset partitioned
by `YEAR` and read with `npc_get_orgs(universe = "registered")` or
`"grassroots"`, which call this function for missing years.

## Usage

``` r
npc_build_registered(
  years,
  filer_source = c("nccs", "archive"),
  dir = npc_cache_dir(),
  filer_dir = NULL,
  refresh = FALSE
)
```

## Arguments

- years:

  Tax years, 2010 onward, within the filer panel's coverage.

- filer_source:

  Where the 990 / 990-EZ filers come from: `"nccs"` (a live build, see
  [`npc_build_nccs()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_build_nccs.md))
  or `"archive"` (the SOI panel archive).

- dir:

  Cache directory; see
  [`npc_cache_dir()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_cache_dir.md).
  Downloads and the registered dataset go here.

- filer_dir:

  Directory of the filer panel: the archive directory for
  `filer_source = "archive"` (default
  [`npc_archive_dir()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_archive_dir.md)),
  or a live build (default `dir`).

- refresh:

  Download the BMF, revocation and e-postcard files again.

## Value

The output directory, invisibly. It holds the Parquet dataset,
`year-counts.csv` (organizations per year by filer type) and
`manifest.csv`.

## Who is registered in year t

The unified BMF records the first and last monthly BMF vintage in which
each EIN appeared. An organization is registered from its ruling year
(or its first BMF vintage, if earlier or the ruling year is unknown)
through the year of its last vintage. Organizations still in the latest
vintage stay registered. Years between an automatic revocation and a
reinstatement are dropped, and a revocation ends the span if it comes
before the organization left the BMF. NCCS kept only three BMF vintages
between August 2016 and January 2022, so exits in those years without a
revocation date are dated to the last vintage seen.

Any organization in the filer panel for year `t` is registered in `t`,
even if its BMF span says otherwise.

## Filer types

- `990`, `990EZ`:

  In the filer panel for `t` (exact tax year).

- `990PF`:

  A private foundation on the BMF (PF filing requirement code 1, or
  foundation code 2-4). Foundations file the 990-PF, which the filer
  panels do not cover, so they have no financial data here.

- `990N`:

  Not a filer or foundation, and either filed a 990-N postcard for `t`
  or has a BMF filing requirement code of 2 (gross receipts normally
  under the 990-N threshold). The postcard file holds only each
  organization's latest filing, so earlier years rely on the BMF code.

- `NONFILER`:

  Everyone else: churches not required to file (`CHURCH = TRUE`, filing
  requirement code 6), dormant organizations not yet revoked, and
  organizations missing from the filer panel.

Imputed gap-year rows of the archive panel do not count as filings; they
are classified like other non-filers and keep `IMPUTED = TRUE`.

Location, subsector, subsection and ruling year come from the BMF for
every row (from the filer panel only when the BMF has no record), so a
filer's county here can differ from its county in the archive.

## See also

[`npc_get_orgs()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_get_orgs.md),
[`npc_registry_spans()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_registry_spans.md)

## Examples

``` r
if (FALSE) { # \dontrun{
npc_build_registered( 2018:2020 )
g <- npc_get_orgs( 2019, universe = "grassroots", source = "nccs" )
table( g$FILER_TYPE )
} # }
```
