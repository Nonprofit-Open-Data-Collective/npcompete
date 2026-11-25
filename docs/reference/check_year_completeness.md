# Flag years with incomplete data

Panels built from IRS filings fill in over several years, so the most
recent tax years (and the first years of a panel) hold only part of the
population. A year is flagged when its row count is below `threshold`
times the typical yearly count. The typical count is the median over
"substantial" years, those with at least 10% of the largest year's
count, so a long tail of nearly empty early years does not drag it down.

## Usage

``` r
check_year_completeness(x, threshold = 0.8)
```

## Arguments

- x:

  Vector of years, one element per organization-year row.

- threshold:

  Share of the typical yearly count below which a year is flagged.

## Value

A data frame with `YEAR`, `N`, `SHARE_OF_MEDIAN` and `INCOMPLETE`.

## Examples

``` r
yrs <- c( rep( 2019, 100 ), rep( 2020, 98 ), rep( 2021, 101 ), rep( 2022, 40 ) )
check_year_completeness( yrs )
#>   YEAR   N SHARE_OF_MEDIAN INCOMPLETE
#> 1 2019 100           1.010      FALSE
#> 2 2020  98           0.990      FALSE
#> 3 2021 101           1.020      FALSE
#> 4 2022  40           0.404       TRUE
```
