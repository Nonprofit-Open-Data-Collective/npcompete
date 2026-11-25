# Commercial share of a market

How much a market relies on earned (program service) revenue rather than
contributions. Replaces the legacy
[`get_density_commercial()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_density_commercial.md).

## Usage

``` r
get_commercial_share(
  df,
  geography = "CENSUS_CBSA_FIPS",
  subsector = "NTEEV2",
  year = "TAXYEAR",
  revenue = "TOTREV",
  program = "REVPROG",
  cutoff = 0.5
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

- revenue:

  Total revenue column.

- program:

  Program service revenue column.

- cutoff:

  An organization is "commercial" when program revenue is more than this
  share of its total revenue (default 0.5).

## Value

A tibble with `GEO`, `SUB`, `YEAR`, `N_COMM` (organizations with
positive total revenue and a non-missing program revenue),
`COMMERCIAL_SHARE` (share of those organizations that are commercial)
and `PROGRAM_REVENUE_SHARE` (program revenue as a share of total revenue
in the market).

## Examples

``` r
cs <- get_commercial_share( npo_panel_sample, geography = "COUNTY_FIPS",
                            year = "TAXYEAR" )
head( cs[ cs$SUB == "ALL", ] )
#> # A tibble: 6 × 6
#>   GEO   SUB    YEAR N_COMM COMMERCIAL_SHARE PROGRAM_REVENUE_SHARE
#>   <chr> <chr> <int>  <int>            <dbl>                 <dbl>
#> 1 50001 ALL    2015    120            0.408                 0.620
#> 2 50001 ALL    2016    118            0.398                 0.686
#> 3 50001 ALL    2017    118            0.390                 0.625
#> 4 50001 ALL    2018    122            0.377                 0.668
#> 5 50001 ALL    2019    123            0.374                 0.628
#> 6 50003 ALL    2015    116            0.362                 0.826
```
