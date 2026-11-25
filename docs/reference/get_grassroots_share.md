# Grassroots share of registered organizations

The share of registered organizations in each market that file a Form
990-N postcard or do not file at all, rather than a Form 990 or 990-EZ.
Use it with the registered universe
(`npc_get_orgs(universe = "registered")`).

## Usage

``` r
get_grassroots_share(
  df,
  geography = "CBSA_FIPS",
  subsector = "NTEEV2",
  year = "YEAR",
  filer_type = "FILER_TYPE"
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

- filer_type:

  Name of the filer type column, with values `"990"`, `"990EZ"`,
  `"990N"` and `"NONFILER"`.

## Value

A tibble with `GEO`, `SUB`, `YEAR`, `N_GRASSROOTS` (990-N filers and
non-filers) and `GRASSROOTS_SHARE`.

## Examples

``` r
d <- data.frame( GEO = c( "A", "A", "A", "B" ), SUB = "ART", YEAR = 2020,
                 FILER_TYPE = c( "990", "990N", "NONFILER", "990EZ" ) )
get_grassroots_share( d, geography = "GEO", subsector = "SUB", year = "YEAR" )
#> # A tibble: 6 × 5
#>   GEO   SUB    YEAR N_GRASSROOTS GRASSROOTS_SHARE
#>   <chr> <chr> <dbl>        <dbl>            <dbl>
#> 1 A     ALL    2020            2            0.667
#> 2 A     ART    2020            2            0.667
#> 3 ALL   ALL    2020            2            0.5  
#> 4 ALL   ART    2020            2            0.5  
#> 5 B     ALL    2020            0            0    
#> 6 B     ART    2020            0            0    
```
