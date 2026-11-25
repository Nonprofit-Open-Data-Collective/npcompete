# Normalized Herfindahl–Hirschman Index

HHI rescaled so that it runs from 0 to 1 whatever the number of
organizations, computed for each geography, subsector and year, with
rollups for all subsectors (`SUB = "ALL"`) and all geographies
(`GEO = "ALL"`).

## Usage

``` r
get_nhhi(
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

  Name of the column that identifies the year, such as `"TAXYEAR"`.

## Value

A tibble with `GEO`, `SUB`, `YEAR`, `N`, the summed resource, `HHI` and
`NHHI`.

## Details

`NHHI = (HHI - 1/N) / (1 - 1/N)`, where `N` is the number of
organizations in the market. The formula is undefined for a single
organization; such markets are monopolies and get `NHHI = 1`, consistent
with `HHI`, `CR1` and `KWOKA`. Markets whose resource total is zero get
`NA`. Filter on `N` to exclude single-organization markets.

## Examples

``` r
toy <- data.frame( GEO = c( "A", "A", "B" ), SUB = "ART", YEAR = 2020,
                   TOTREV = c( 75, 25, 10 ) )
get_nhhi( toy, geography = "GEO", subsector = "SUB", year = "YEAR" )
#> # A tibble: 6 × 7
#>   GEO   SUB    YEAR     N TOTREV   HHI  NHHI
#>   <chr> <chr> <dbl> <int>  <dbl> <dbl> <dbl>
#> 1 A     ALL    2020     2    100 0.625 0.25 
#> 2 A     ART    2020     2    100 0.625 0.25 
#> 3 ALL   ALL    2020     3    110 0.525 0.287
#> 4 ALL   ART    2020     3    110 0.525 0.287
#> 5 B     ALL    2020     1     10 1     1    
#> 6 B     ART    2020     1     10 1     1    
```
