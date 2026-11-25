# Herfindahl–Hirschman Index

Market concentration in each geography, subsector and year, with rollups
for all subsectors (`SUB = "ALL"`) and all geographies (`GEO = "ALL"`).

## Usage

``` r
get_hhi(
  df,
  geography = "CENSUS_CBSA_FIPS",
  subsector = "NTEEV2",
  resource = "TOTREV",
  year = "TAXYEAR"
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

  Name of the year column, or `NULL` to pool all years.

## Value

A tibble with `GEO`, `SUB`, `YEAR` (unless `year = NULL`), `N`, the
summed resource and `HHI`. Markets whose resource total is zero get
`HHI = NA`.

## Details

HHI is the sum of squared market shares, where an organization's share
is its resource divided by the market total (Thornton & Belski, 2010).
It ranges from 1/N to 1; a market with an HHI of 1 is a monopoly and
values near 0 indicate many similar-sized competitors.

## Examples

``` r
toy <- data.frame( GEO = c( "A", "A", "B" ), SUB = "ART", YEAR = 2020,
                   TOTREV = c( 75, 25, 10 ) )
get_hhi( toy, geography = "GEO", subsector = "SUB", year = "YEAR" )
#> # A tibble: 6 × 6
#>   GEO   SUB    YEAR     N TOTREV   HHI
#>   <chr> <chr> <dbl> <int>  <dbl> <dbl>
#> 1 A     ALL    2020     2    100 0.625
#> 2 A     ART    2020     2    100 0.625
#> 3 ALL   ALL    2020     3    110 0.525
#> 4 ALL   ART    2020     3    110 0.525
#> 5 B     ALL    2020     1     10 1    
#> 6 B     ART    2020     1     10 1    
```
