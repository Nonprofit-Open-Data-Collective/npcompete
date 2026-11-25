# Median organization age

Median years since the IRS ruling that granted exemption, among the
organizations in each market.

## Usage

``` r
get_median_age(
  df,
  geography = "CENSUS_CBSA_FIPS",
  subsector = "NTEEV2",
  year = "TAXYEAR",
  ruling = "RULING_YEAR"
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

- ruling:

  Name of the ruling-year column.

## Value

A tibble with `GEO`, `SUB`, `YEAR`, `N_AGE` (organizations with a valid
ruling year) and `MEDIAN_AGE`. Ruling years after the tax year are
treated as missing.

## Examples

``` r
a <- get_median_age( npo_panel_sample, geography = "COUNTY_FIPS",
                     year = "TAXYEAR", ruling = "ORG_RULING_YEAR" )
head( a[ a$SUB == "ALL", ] )
#> # A tibble: 6 × 5
#>   GEO   SUB    YEAR N_AGE MEDIAN_AGE
#>   <chr> <chr> <int> <int>      <dbl>
#> 1 50001 ALL    2015   118         22
#> 2 50001 ALL    2016   119         21
#> 3 50001 ALL    2017   119         22
#> 4 50001 ALL    2018   120         23
#> 5 50001 ALL    2019   126         23
#> 6 50003 ALL    2015   116         22
```
