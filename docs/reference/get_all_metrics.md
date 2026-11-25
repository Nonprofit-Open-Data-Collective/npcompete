# Compute all competition metrics in one table

Runs
[`get_gini()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_gini.md),
[`get_nhhi()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_nhhi.md),
[`get_kwoka()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_kwoka.md),
[`get_cr_all()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_cr_all.md)
and
[`get_scale_free_density()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_scale_free_density.md)
and joins the results, one row per geography, subsector and year (plus
`ALL` rollups).

## Usage

``` r
get_all_metrics(
  df,
  geography = "CENSUS_CBSA_FIPS",
  population = "POP_METRO",
  subsector = "NTEEV2",
  resource = "TOTREV",
  year = "TAXYEAR",
  cr_n = 1:8,
  pop_table = NULL,
  density_model = c("negbin", "poisson", "loglinear"),
  drop_incomplete_years = TRUE
)
```

## Arguments

- df:

  A data frame with one row per organization-year.

- geography:

  Name of the column that identifies the market's geography, such as
  `"CENSUS_CBSA_FIPS"` or `"COUNTY_FIPS"`.

- population:

  Name of the column that holds the market's population, such as
  `"POP_METRO"` or `"POP_COUNTY"`. Ignored, and may be `NULL`, when
  `pop_table` is supplied.

- subsector:

  Name of the column that identifies the subsector. When it is
  `"NTEEV2"`, the first three characters (the major group) are used.

- resource:

  Name of the numeric column that measures organization size, such as
  `"TOTREV"`, `"TOTEXP"` or `"NETASS"`. Negative values are set to zero.

- year:

  Name of the column that identifies the year, such as `"TAXYEAR"`.

- cr_n:

  Integer vector of concentration ratio levels to compute.

- pop_table:

  Optional data frame of market populations with columns `GEO`, `YEAR`
  and `POP`. Supply it so that markets with no organizations at all are
  still included. When `NULL`, market populations are taken from the
  `population` column of `df`, so only markets with at least one
  organization (in any subsector) are included.

- density_model:

  Passed to
  [`get_scale_free_density()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_scale_free_density.md)
  as `model`.

- drop_incomplete_years:

  If `TRUE` (the default), years flagged by
  [`check_year_completeness()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/check_year_completeness.md)
  are dropped with a message before any metric is computed.

## Value

A tibble with `GEO`, `SUB`, `YEAR`, `N`, `POP`, the summed resource,
`LOG_NPO`, `LOG_POP`, `LOG_<resource>`, `DENSITY_PCTL_N`,
`DENSITY_PCTL_R`, `GINI`, `HHI`, `NHHI`, `KWOKA` and `CR1` to `CR8`.
Market cells with no organizations (from the density frame) have `N = 0`
and `NA` concentration metrics.

## Examples

``` r
m <- get_all_metrics( npo_panel_sample,
                      geography = "COUNTY_FIPS",
                      population = "POP_COUNTY",
                      resource = "TOTREV" )
#> Calculating Gini...
#> Calculating normalized HHI...
#> Calculating Kwoka index...
#> Calculating concentration ratios...
#> Calculating nonprofit density...
head( m )
#> # A tibble: 6 × 23
#>   GEO   SUB    YEAR     N    POP    TOTREV LOG_NPO LOG_POP LOG_TOTREV
#>   <chr> <chr> <int> <int>  <dbl>     <dbl>   <dbl>   <dbl>      <dbl>
#> 1 50001 ALL    2015   121 37092  609899321    4.83    10.5       20.2
#> 2 50001 ALL    2016   118 37146. 582431292    4.81    10.5       20.2
#> 3 50001 ALL    2017   119 37200. 656375751    4.81    10.5       20.3
#> 4 50001 ALL    2018   122 37255. 642370035    4.82    10.5       20.3
#> 5 50001 ALL    2019   123 37309. 655600800    4.84    10.5       20.3
#> 6 50001 ART    2015    19 37092   25718965    3.00    10.5       17.1
#> # ℹ 14 more variables: DENSITY_PCTL_N <dbl>, DENSITY_PCTL_R <dbl>, GINI <dbl>,
#> #   HHI <dbl>, NHHI <dbl>, KWOKA <dbl>, CR1 <dbl>, CR2 <dbl>, CR3 <dbl>,
#> #   CR4 <dbl>, CR5 <dbl>, CR6 <dbl>, CR7 <dbl>, CR8 <dbl>
```
