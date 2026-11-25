# Concentration ratio (CRk)

Convenience wrapper around
[`get_cr_all()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_cr_all.md)
for a single CR level.

## Usage

``` r
get_cr(
  df,
  geography = "CENSUS_CBSA_FIPS",
  subsector = "NTEEV2",
  resource = "TOTREV",
  year = "TAXYEAR",
  number = 4
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

- resource:

  Name of the numeric column that measures organization size, such as
  `"TOTREV"`, `"TOTEXP"` or `"NETASS"`. Negative values are set to zero.

- year:

  Name of the column that identifies the year, such as `"TAXYEAR"`.

- number:

  Number of top organizations to include, for example `4` for CR4.

## Value

A tibble with `GEO`, `SUB`, `YEAR`, `N`, the summed resource and
`CR<number>`.

## Examples

``` r
toy <- data.frame( GEO = "A", SUB = "ART", YEAR = 2020,
                   TOTREV = c( 50, 30, 10, 10 ) )
get_cr( toy, geography = "GEO", subsector = "SUB", year = "YEAR", number = 2 )
#> # A tibble: 4 × 6
#>   GEO   SUB    YEAR     N TOTREV   CR2
#>   <chr> <chr> <dbl> <int>  <dbl> <dbl>
#> 1 A     ALL    2020     4    100   0.8
#> 2 A     ART    2020     4    100   0.8
#> 3 ALL   ALL    2020     4    100   0.8
#> 4 ALL   ART    2020     4    100   0.8
```
