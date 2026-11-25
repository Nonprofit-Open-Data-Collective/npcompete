# Build competition and density metrics for any set of years

Reads organizations for the requested years, attaches market
populations, and computes every metric from
[`get_all_metrics()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_all_metrics.md)
for each geography level and resource. Returns a tidy long table (or the
familiar wide tables) and can write the results with a manifest of what
was used.

## Usage

``` r
npc_build_metrics(
  years,
  geo = c("cbsa", "county"),
  resources = c("TOTREV", "TOTEXP", "NETASS"),
  universe = c("filers", "registered", "grassroots"),
  source = c("archive", "nccs"),
  dir = npc_archive_dir(),
  subsection = 3,
  include_imputed = TRUE,
  cbsa = c("crosswalk", "geocode"),
  density_model = c("negbin", "poisson", "loglinear"),
  cr_n = 1:8,
  drop_incomplete_years = TRUE,
  format = c("long", "wide"),
  out_dir = NULL
)
```

## Arguments

- years:

  Integer vector of tax years, for example `2012:2022`.

- geo:

  Geography levels: `"cbsa"`, `"county"`, or both.

- resources:

  Resource columns for the concentration metrics, any of `"TOTREV"`,
  `"TOTEXP"`, `"NETASS"`, `"TOTASS"`.

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

- density_model:

  Passed to
  [`get_scale_free_density()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_scale_free_density.md)
  as `model`.

- cr_n:

  Integer vector of concentration ratio levels to compute.

- drop_incomplete_years:

  Drop requested years that hold a partial panel, judged against the
  year counts of the whole archive (see
  [`npc_archive_info()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_archive_info.md)).

- format:

  `"long"` (one row per market, metric and resource) or `"wide"` (a
  named list of tables, one per geography and resource, in the layout of
  [`get_all_metrics()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_all_metrics.md)).

- out_dir:

  Optional directory. When set, results are written there as CSV along
  with `manifest.csv`.

## Value

For `format = "long"`, a tibble with `GEO_LEVEL`, `GEO`, `SUB`, `YEAR`,
`UNIVERSE`, `RESOURCE`, `METRIC` and `VALUE`. For `format = "wide"`, a
named list of tibbles such as `cbsa_TOTREV`. Both carry a `"manifest"`
attribute describing the build.

Rows with `GEO = "ALL"` are national: they include organizations outside
any market of that level (for example, outside every CBSA), and their
`POP` is the U.S. population from
[pop_county](https://nonprofit-open-data-collective.github.io/npcompete/reference/pop_county.md).

## Examples

``` r
if (FALSE) { # \dontrun{
options( npcompete.archive_dir = "D:/data/npcompete-archive" )

m <- npc_build_metrics( 2015:2020, geo = "cbsa", resources = "TOTREV" )
subset( m, GEO == "35620" & SUB == "ALL" & METRIC == "HHI" )

# Same tables as the original build script, written to disk
npc_build_metrics( 2011:2022, format = "wide", out_dir = "metrics" )
} # }
```
