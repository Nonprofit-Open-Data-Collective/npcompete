# Scale-free nonprofit density

Measures whether a market has more or fewer nonprofits (and more or less
of a resource) than expected for its population. Within each subsector
and year, the number of nonprofits and the resource total are modeled as
a power function of population across markets, and each market's
observed value is compared with the value expected for its size.

## Usage

``` r
get_scale_free_density(
  df,
  geography = "CENSUS_CBSA_FIPS",
  population = "POP_METRO",
  subsector = "NTEEV2",
  year = "TAXYEAR",
  resource = "TOTREV",
  pop_table = NULL,
  model = c("negbin", "poisson", "loglinear")
)
```

## Arguments

- df:

  A data frame with one row per organization-year.

- geography:

  Name of the column that identifies the market's geography, such as
  `"CENSUS_CBSA_FIPS"` or `"COUNTY_FIPS"`.

- population:

  Name of the column that holds the market's population, such as
  `"POP_METRO"` or `"POP_COUNTY"`. Ignored, and may be `NULL`, when
  `pop_table` is supplied.

- subsector:

  Name of the column that identifies the subsector. When it is
  `"NTEEV2"`, the first three characters (the major group) are used.

- year:

  Name of the column that identifies the year, such as `"TAXYEAR"`.

- resource:

  Name of the numeric column that measures organization size, such as
  `"TOTREV"`, `"TOTEXP"` or `"NETASS"`. Negative values are set to zero.

- pop_table:

  Optional data frame of market populations with columns `GEO`, `YEAR`
  and `POP`. Supply it so that markets with no organizations at all are
  still included. When `NULL`, market populations are taken from the
  `population` column of `df`, so only markets with at least one
  organization (in any subsector) are included.

- model:

  How the expected values are estimated:

  - `"negbin"` (default): negative binomial regression of the count on
    log population, which handles zero counts and overdispersion. Falls
    back to Poisson when the negative binomial fit fails.

  - `"poisson"`: Poisson regression of the count on log population.

  - `"loglinear"`: least squares regression of `log(N + 1)` on
    `log(POP + 1)`, the original method.

  For the resource total, both count options use Poisson pseudo-maximum
  likelihood (a quasi-Poisson GLM), which is suited to non-negative
  dollar amounts with many zeros.

## Value

A tibble with one row per `GEO`, `SUB` and `YEAR`, containing `N`,
`POP`, the summed resource, `LOG_NPO`, `LOG_POP`, `LOG_<resource>`,
`EXPECTED_N`, `EXPECTED_<resource>`, `DENSITY_PCTL_N`, `DENSITY_PCTL_R`
(both capped at 2) and the fitted models in the list columns `model_n`
and `model_r`. The model type is stored in the `"density_model"`
attribute.

Rows with `SUB = "ALL"` cover all subsectors in a market. Rows with
`GEO = "ALL"` are national totals: their `POP` is the summed population
of the markets, and their density values are `NA` because they are not
part of the cross-market models.

## Details

The density index is `observed / expected - 1`: 0 means the market
matches the expected density for its size, 0.5 means 50% more than
expected, and -1 means none at all. The `"loglinear"` model computes it
as `exp(residual) - 1` on the `log(x + 1)` scale, which compresses
differences among small markets.

Every market in the population frame appears in every subsector, with
`N = 0` where it has no organizations, so the models are not limited to
markets that happen to have an organization in that subsector.
Organizations are counted whether or not their resource value is
missing; the resource total ignores missing values.

## Examples

``` r
d <- get_scale_free_density( npo_panel_sample,
                             geography = "COUNTY_FIPS",
                             population = "POP_COUNTY" )
head( d[ d$SUB == "ALL", c( "GEO", "YEAR", "N", "EXPECTED_N", "DENSITY_PCTL_N" ) ] )
#> # A tibble: 6 × 5
#>   GEO    YEAR     N EXPECTED_N DENSITY_PCTL_N
#>   <chr> <int> <int>      <dbl>          <dbl>
#> 1 50001  2015   124       102.          0.216
#> 2 50001  2016   122       103.          0.184
#> 3 50001  2017   122       105.          0.165
#> 4 50001  2018   123       106.          0.161
#> 5 50001  2019   126       106.          0.189
#> 6 50003  2015   121       102.          0.181
```
