# Gini coefficient

Inequality of the resource distribution in each geography, subsector and
year, with rollups for all subsectors (`SUB = "ALL"`) and all
geographies (`GEO = "ALL"`).

## Usage

``` r
get_gini(
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
summed resource and `GINI`. Markets with a single organization get
`GINI = 1`.

## Details

The Gini coefficient is a widely used measure of inequality (Dorfman,
1979). Low values indicate less inequality and more competition; high
values indicate greater inequality. Seaman et al. (2014) used it
alongside HHI and suggested reading values below 0.5 as reasonable
equality, 0.5 to 0.75 as modest inequality, and above 0.75 as high
inequality. Computed with
[`DescTools::Gini()`](https://andrisignorell.github.io/DescTools/reference/Gini.html).

## Examples

``` r
toy <- data.frame( GEO = c( "A", "A", "A", "B" ), SUB = "ART", YEAR = 2020,
                   TOTREV = c( 10, 20, 70, 5 ) )
get_gini( toy, geography = "GEO", subsector = "SUB", year = "YEAR" )
#> # A tibble: 6 × 6
#>   GEO   SUB    YEAR     N TOTREV  GINI
#>   <chr> <chr> <dbl> <int>  <dbl> <dbl>
#> 1 A     ALL    2020     3    100 0.6  
#> 2 A     ART    2020     3    100 0.6  
#> 3 ALL   ALL    2020     4    105 0.651
#> 4 ALL   ART    2020     4    105 0.651
#> 5 B     ALL    2020     1      5 1    
#> 6 B     ART    2020     1      5 1    
```
