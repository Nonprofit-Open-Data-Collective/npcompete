# Read organization-years for a set of years

Returns organizations in the canonical schema (see
[`npc_standardize()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_standardize.md))
for the requested years, with `CBSA_FIPS` attached.

## Usage

``` r
npc_get_orgs(
  years,
  universe = c("filers", "registered", "grassroots"),
  source = c("archive", "nccs"),
  dir = npc_archive_dir(),
  subsection = 3,
  include_imputed = TRUE,
  cbsa = c("crosswalk", "geocode"),
  columns = NULL
)
```

## Arguments

- years:

  Integer vector of tax years.

- universe:

  Which organizations: `"filers"` (Form 990 and 990-EZ filers). The
  registered and grassroots universes are planned.

- source:

  `"archive"` reads the local Parquet archive. Downloading from NCCS
  (`"nccs"`) is planned.

- dir:

  Archive directory; see
  [`npc_archive_dir()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_archive_dir.md).

- subsection:

  501(c) subsections to keep, or `NULL` for all.

- include_imputed:

  Keep rows that fill gap years between filings. They have no financial
  data but mark years when the organization existed.

- cbsa:

  How to assign CBSAs: `"crosswalk"` (default) maps each county to its
  CBSA under the 2023 delineation
  ([cbsa_county_xwalk](https://nonprofit-open-data-collective.github.io/npcompete/reference/cbsa_county_xwalk.md)),
  matching the population data; `"geocode"` uses the CBSA assigned when
  the address was geocoded.

- columns:

  Optional character vector of columns to return.

## Value

A data frame of organization-years.

## Examples

``` r
if (FALSE) { # \dontrun{
orgs <- npc_get_orgs( 2018:2020 )
} # }
```
