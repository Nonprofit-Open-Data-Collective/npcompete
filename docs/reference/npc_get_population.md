# Market populations for a geography level

Market populations for a geography level

## Usage

``` r
npc_get_population(geo = c("county", "cbsa"), years = NULL)
```

## Arguments

- geo:

  `"county"` or `"cbsa"`.

- years:

  Optional years to keep.

## Value

A data frame with `GEO`, `YEAR` and `POP`, ready to pass as `pop_table`
to
[`get_scale_free_density()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_scale_free_density.md)
or
[`get_all_metrics()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_all_metrics.md).

## Examples

``` r
head( npc_get_population( "cbsa", 2020 ) )
#>     GEO YEAR    POP
#> 1 10100 2020  42287
#> 2 10140 2020  75636
#> 3 10180 2020 176579
#> 4 10220 2020  38065
#> 5 10300 2020  99423
#> 6 10380 2020 253768
```
