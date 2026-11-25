# Build competition and density metrics for any set of years

Reads organizations for the requested years, attaches market
populations, and computes the requested metrics (see
[`npc_metrics()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_metrics.md))
for each geography level and resource. Returns a tidy long table (or
wide tables) and can write the results with a manifest of what was used.

## Usage

``` r
npc_build_metrics(
  years,
  geo = c("cbsa", "county"),
  resources = c("TOTREV", "TOTEXP", "NETASS"),
  metrics = "default",
  universe = c("filers", "registered", "grassroots"),
  source = c("archive", "nccs"),
  dir = NULL,
  subsection = 3,
  include_imputed = TRUE,
  cbsa = c("crosswalk", "geocode"),
  window = 1,
  density_model = c("negbin", "poisson", "loglinear"),
  small_max = 1e+05,
  large_min = 1e+07,
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

  Resource columns for the resource-based metrics, any of `"TOTREV"`,
  `"TOTEXP"`, `"NETASS"`, `"TOTASS"`.

- metrics:

  Metric ids from
  [`npc_metrics()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_metrics.md),
  or `"default"` for every metric valid for `universe`.

- universe:

  Which organizations: `"filers"` (Form 990 and 990-EZ filers). The
  registered and grassroots universes are planned.

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
  data but mark years when the organization existed.

- cbsa:

  How to assign CBSAs: `"crosswalk"` (default) maps each county to its
  CBSA under the 2023 delineation
  ([cbsa_county_xwalk](https://nonprofit-open-data-collective.github.io/npcompete/reference/cbsa_county_xwalk.md)),
  matching the population data; `"geocode"` uses the CBSA assigned when
  the address was geocoded (archive only).

- window:

  Number of years pooled for each reported year. With `window = 3`, the
  value for 2020 uses 2018-2020: each organization's financial values
  are averaged over the years it appears, its location and subsector
  come from its latest year, and population is the three-year average.
  Counts are then organizations active at any point in the window. Use
  it to steady metrics for small markets.

- density_model:

  Model for `DENSITY_PCTL_N` and `DENSITY_PCTL_R`; passed to
  [`get_scale_free_density()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_scale_free_density.md)
  as `model`.

- small_max:

  Organizations with a resource value below this are "small". The
  default is \$100,000.

- large_min:

  Organizations with a resource value at or above this are "large". The
  default is \$10 million.

- drop_incomplete_years:

  Drop reported years whose data (including the earlier years of a
  window) hold a partial panel, judged against the year counts of the
  whole archive or live build (see
  [`npc_archive_info()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_archive_info.md)).

- format:

  `"long"` (one row per market, metric and resource) or `"wide"` (a
  named list of tables, one per geography and resource).

- out_dir:

  Optional directory. When set, results are written there as CSV along
  with `manifest.csv`.

## Value

For `format = "long"`, a tibble with `GEO_LEVEL`, `GEO`, `SUB`, `YEAR`,
`UNIVERSE`, `RESOURCE`, `METRIC` and `VALUE`. Metrics that do not depend
on the resource (counts, age, commercial share, ...) appear once, with
`RESOURCE = NA`. For `format = "wide"`, a named list of tibbles such as
`cbsa_TOTREV` (or `cbsa` when no requested metric depends on the
resource). Both carry a `"manifest"` attribute describing the build.

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

# Density and age only, three-year windows, county level
npc_build_metrics( 2014:2022, geo = "county", window = 3,
                   metrics = c( "DENSITY_PER_100K", "MEDIAN_AGE" ) )

# Live build from NCCS downloads (cached under npc_cache_dir())
npc_build_metrics( 2019:2021, geo = "cbsa", resources = "TOTREV",
                   source = "nccs" )
} # }
```
