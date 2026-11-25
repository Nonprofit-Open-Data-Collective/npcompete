# Kwoka dominance index

Kwoka's (1977) dominance index for each geography, subsector and year,
with rollups for all subsectors (`SUB = "ALL"`) and all geographies
(`GEO = "ALL"`). It measures how far the largest organizations pull
ahead of the next ones.

## Usage

``` r
get_kwoka(
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

A tibble with `GEO`, `SUB`, `YEAR`, `N`, the shares of the three largest
organizations (`top1`, `top2`, `top3`; `NA` when the market has fewer
organizations) and `KWOKA`.

## Details

With market shares sorted from largest to smallest, \$\$KWOKA =
\sum\_{i=1}^{N-1} (s_i - s\_{i+1})^2\$\$ The index is 0 when all
organizations have equal shares and approaches 1 as one organization
dominates. A market with a single organization gets `KWOKA = 1`; a
market whose resource total is zero gets `NA`.

## References

Kwoka, J. E. (1977). Large firm dominance and price-cost margins in
manufacturing industries. *Southern Economic Journal*, 44(1), 183-189.

## Examples

``` r
toy <- data.frame( GEO = "A", SUB = "ART", YEAR = 2020,
                   TOTREV = c( 50, 30, 10, 10 ) )
get_kwoka( toy, geography = "GEO", subsector = "SUB", year = "YEAR" )
#> # A tibble: 4 × 8
#>   GEO   SUB    YEAR     N  top1  top2  top3 KWOKA
#>   <chr> <chr> <dbl> <int> <dbl> <dbl> <dbl> <dbl>
#> 1 A     ALL    2020     4   0.5   0.3   0.1  0.08
#> 2 A     ART    2020     4   0.5   0.3   0.1  0.08
#> 3 ALL   ALL    2020     4   0.5   0.3   0.1  0.08
#> 4 ALL   ART    2020     4   0.5   0.3   0.1  0.08
```
