# Nonprofit density per capita, overall and by size

Counts organizations in each market and divides by population. When a
resource column is given, it also counts small and large organizations.
Every market in `pop_table` appears in every subsector, with zero counts
where it has no organizations.

## Usage

``` r
get_density_per_capita(
  df,
  geography = "CENSUS_CBSA_FIPS",
  subsector = "NTEEV2",
  year = "TAXYEAR",
  pop_table,
  resource = NULL,
  small_max = 1e+05,
  large_min = 1e+07,
  per = 1e+05
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

- pop_table:

  Data frame of market populations with columns `GEO`, `YEAR` and `POP`,
  for example from
  [`npc_get_population()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_get_population.md).

- resource:

  Optional resource column used to classify organizations by size.
  `NULL` gives counts only.

- small_max:

  Organizations with a resource value below this are "small". The
  default is \$100,000.

- large_min:

  Organizations with a resource value at or above this are "large". The
  default is \$10 million.

- per:

  Population base for the rates (default 100,000).

## Value

A tibble with `GEO`, `SUB`, `YEAR`, `POP`, `N` and `DENSITY_PER_100K`.
With a resource, it also has `N_RES` (organizations with a non-missing
value), `N_SMALL`, `N_LARGE`, `SMALL_DENSITY_PER_100K`,
`LARGE_DENSITY_PER_100K`, `SMALL_SHARE` and `LARGE_SHARE` (shares of
`N_RES`). Rows with `GEO = "ALL"` are national; their `POP` is the
summed population of the markets in `pop_table`. Markets that have
organizations but are missing from `pop_table` are kept with `POP = NA`.

## Details

The column suffix stays `_PER_100K` for any `per`; the `per` value is
stored in the `"per"` attribute.

## Examples

``` r
d <- get_density_per_capita( npo_panel_sample, geography = "COUNTY_FIPS",
                             year = "TAXYEAR",
                             pop_table = npc_get_population( "county", 2015:2019 ),
                             resource = "TOTREV" )
head( d[ d$SUB == "ALL", ] )
#> # A tibble: 6 × 13
#>   GEO   SUB    YEAR     POP     N N_RES N_SMALL N_LARGE DENSITY_PER_100K
#>   <chr> <chr> <dbl>   <dbl> <dbl> <dbl>   <dbl>   <dbl>            <dbl>
#> 1 01001 ALL    2015  56688      0     0       0       0                0
#> 2 01001 ALL    2016  57111.     0     0       0       0                0
#> 3 01001 ALL    2017  57535.     0     0       0       0                0
#> 4 01001 ALL    2018  57958.     0     0       0       0                0
#> 5 01001 ALL    2019  58382.     0     0       0       0                0
#> 6 01003 ALL    2015 207016      0     0       0       0                0
#> # ℹ 4 more variables: SMALL_DENSITY_PER_100K <dbl>,
#> #   LARGE_DENSITY_PER_100K <dbl>, SMALL_SHARE <dbl>, LARGE_SHARE <dbl>
```
