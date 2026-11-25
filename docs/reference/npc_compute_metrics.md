# Compute a set of metrics from the registry

Runs only the metric families needed for the requested metrics and joins
them onto a market frame that has every market in `pop_table`, with zero
counts where a market has no organizations.

## Usage

``` r
npc_compute_metrics(
  df,
  geography = "CBSA_FIPS",
  subsector = "NTEEV2",
  year = "YEAR",
  resource = "TOTREV",
  pop_table,
  metrics = "default",
  universe = "filers",
  cr_n = 1:8,
  density_model = c("negbin", "poisson", "loglinear"),
  small_max = 1e+05,
  large_min = 1e+07,
  ruling = "RULING_YEAR",
  program = "REVPROG",
  contributions = "REVCONT",
  fees = "FNDFEE"
)
```

## Arguments

- df:

  A data frame with one row per organization-year.

- geography:

  Name of the column that identifies the market's geography, such as
  `"CENSUS_CBSA_FIPS"` or `"COUNTY_FIPS"`.

- subsector:

  Name of the column that identifies the subsector. When it is
  `"NTEEV2"`, the first three characters (the major group) are used.

- year:

  Name of the column that identifies the year, such as `"TAXYEAR"`.

- resource:

  Name of the numeric column that measures organization size, such as
  `"TOTREV"`, `"TOTEXP"` or `"NETASS"`. Negative values are set to zero.

- pop_table:

  Data frame of market populations with columns `GEO`, `YEAR` and `POP`,
  for example from
  [`npc_get_population()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_get_population.md).

- metrics:

  Metric ids from
  [`npc_metrics()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_metrics.md),
  or `"default"` for every metric valid for `universe`.

- universe:

  Universe the data represent; used to check that the requested metrics
  are valid for it.

- cr_n:

  Not used; concentration ratios are chosen through `metrics` (`"CR1"`
  to `"CR8"`). Kept for compatibility with
  [`get_all_metrics()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_all_metrics.md).

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

- ruling, program, contributions, fees:

  Columns for the ruling year, program service revenue, contributions
  and professional fundraising fees.

## Value

A tibble with `GEO`, `SUB`, `YEAR`, `N` (all organizations), `POP` and
the requested metrics. Supporting counts are kept where relevant:
`N_RES` and the resource total for resource metrics, `N_SMALL` /
`N_LARGE`, `N_AGE`, `N_COMM` and `N_FEE`.

## Examples

``` r
m <- npc_compute_metrics( npo_panel_sample, geography = "COUNTY_FIPS",
                          year = "TAXYEAR", ruling = "ORG_RULING_YEAR",
                          pop_table = npc_get_population( "county", 2015:2019 ),
                          metrics = c( "DENSITY_PER_100K", "HHI", "MEDIAN_AGE" ) )
head( m[ m$SUB == "ALL", ] )
#> # A tibble: 6 × 11
#>   GEO   SUB    YEAR     N     POP N_RES TOTREV N_AGE DENSITY_PER_100K MEDIAN_AGE
#>   <chr> <chr> <dbl> <dbl>   <dbl> <dbl>  <dbl> <int>            <dbl>      <dbl>
#> 1 01001 ALL    2015     0  56688      0     NA     0                0         NA
#> 2 01001 ALL    2016     0  57111.     0     NA     0                0         NA
#> 3 01001 ALL    2017     0  57535.     0     NA     0                0         NA
#> 4 01001 ALL    2018     0  57958.     0     NA     0                0         NA
#> 5 01001 ALL    2019     0  58382.     0     NA     0                0         NA
#> 6 01003 ALL    2015     0 207016      0     NA     0                0         NA
#> # ℹ 1 more variable: HHI <dbl>
```
