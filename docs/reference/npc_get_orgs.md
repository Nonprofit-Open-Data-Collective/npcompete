# Read organization-years for a set of years

Returns organizations in the canonical schema (see
[`npc_standardize()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_standardize.md))
for the requested years, with `CBSA_FIPS` attached, and one row per EIN
and tax year. Archives built before duplicates were removed in
[`npc_standardize()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_standardize.md)
are deduplicated the same way when read.

## Usage

``` r
npc_get_orgs(
  years,
  universe = c("filers", "registered", "grassroots"),
  source = c("archive", "nccs"),
  dir = NULL,
  subsection = 3,
  include_imputed = TRUE,
  exclude_churches = FALSE,
  cbsa = c("crosswalk", "geocode"),
  columns = NULL
)
```

## Arguments

- years:

  Integer vector of tax years.

- universe:

  Which organizations:

  `"filers"`

  :   Form 990 and 990-EZ filers, with financial data.

  `"registered"`

  :   Every organization on the IRS registry that year, with
      `FILER_TYPE` `"990"`, `"990EZ"`, `"990PF"`, `"990N"` or
      `"NONFILER"` (see
      [`npc_build_registered()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_build_registered.md)).
      Financial columns are filled for filers only.

  `"grassroots"`

  :   Registered organizations that did not file a 990 or 990-EZ:
      `FILER_TYPE` `"990N"` or `"NONFILER"`.

  The registered and grassroots universes add `FILING_REQ_CODE`,
  `FOUNDATION` and `CHURCH`, and are built on first use under
  [`npc_cache_dir()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_cache_dir.md),
  with the filers taken from `source`.

- source:

  `"archive"` reads the local Parquet archive built from the SOI panel.
  `"nccs"` reads a live build from NCCS downloads (see
  [`npc_build_nccs()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_build_nccs.md)),
  downloading and building any missing years first.

- dir:

  Archive directory for `source = "archive"` (default
  [`npc_archive_dir()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_archive_dir.md))
  or cache directory for `source = "nccs"` (default
  [`npc_cache_dir()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_cache_dir.md)).

- subsection:

  501(c) subsections to keep, or `NULL` for all.

- include_imputed:

  Keep rows that fill gap years between filings. They have no financial
  data but mark years when the organization existed. Filers universe
  only.

- exclude_churches:

  Drop churches that are not required to file (BMF filing requirement
  code 6). Registered and grassroots universes only.

- cbsa:

  How to assign CBSAs: `"crosswalk"` (default) maps each county to its
  CBSA under the 2023 delineation
  ([cbsa_county_xwalk](https://nonprofit-open-data-collective.github.io/npcompete/reference/cbsa_county_xwalk.md)),
  matching the population data; `"geocode"` uses the CBSA assigned when
  the address was geocoded (archive filers only).

- columns:

  Optional character vector of columns to return.

## Value

A data frame of organization-years.

## Examples

``` r
if (FALSE) { # \dontrun{
orgs <- npc_get_orgs( 2018:2020 )
grassroots <- npc_get_orgs( 2019, universe = "grassroots", source = "nccs" )
} # }
```
