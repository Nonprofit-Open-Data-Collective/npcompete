# Concentration ratios CR1 to CRk

Share of a market's total resource held by its largest organizations,
computed for each geography, subsector and year, with rollups for all
subsectors (`SUB = "ALL"`) and all geographies (`GEO = "ALL"`).

## Usage

``` r
get_cr_all(
  df,
  geography = "CENSUS_CBSA_FIPS",
  subsector = "NTEEV2",
  resource = "TOTREV",
  year = "TAXYEAR",
  numbers = 1:8,
  as_percent = FALSE
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

- numbers:

  Integer vector of CR levels to calculate, for example `c(1, 4, 8)`.
  Values above 8 produce a warning.

- as_percent:

  If `TRUE`, ratios are returned on a 0-100 scale instead of 0-1.

## Value

A tibble with `GEO`, `SUB`, `YEAR`, `N`, the summed resource and one
column per requested ratio (`CR1`, `CR2`, ...). Markets with k or fewer
organizations get `CRk = 1`.

## Details

CRk is the sum of the resource held by the k largest organizations
divided by the market total.

## Examples

``` r
toy <- data.frame( GEO = "A", SUB = "ART", YEAR = 2020,
                   TOTREV = c( 50, 30, 10, 10 ) )
get_cr_all( toy, geography = "GEO", subsector = "SUB", year = "YEAR",
            numbers = 1:3 )
#> # A tibble: 4 × 8
#>   GEO   SUB    YEAR     N TOTREV   CR1   CR2   CR3
#>   <chr> <chr> <dbl> <int>  <dbl> <dbl> <dbl> <dbl>
#> 1 A     ALL    2020     4    100   0.5   0.8   0.9
#> 2 A     ART    2020     4    100   0.5   0.8   0.9
#> 3 ALL   ALL    2020     4    100   0.5   0.8   0.9
#> 4 ALL   ART    2020     4    100   0.5   0.8   0.9
```
