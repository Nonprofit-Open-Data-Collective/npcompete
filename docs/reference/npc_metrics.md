# List the available metrics

The metric registry: every metric
[`npc_compute_metrics()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_compute_metrics.md)
and
[`npc_build_metrics()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_build_metrics.md)
can produce, which universes of organizations it is valid for, and
whether it needs financial data.

## Usage

``` r
npc_metrics(universe = NULL)
```

## Arguments

- universe:

  Optional universe (`"filers"`, `"registered"` or `"grassroots"`) to
  list only the metrics valid for it.

## Value

A data frame with `id`, `family`, `financial` (needs Form 990 or 990-EZ
financial data), `by_resource` (depends on the resource column),
`universes` and `description`.

## Details

Financial metrics are limited to filers: organizations that file the
Form 990-N postcard or file nothing do not report revenue, expenses or
assets.

## Examples

``` r
npc_metrics()
#>                        id      family financial by_resource
#> 1        DENSITY_PER_100K       count     FALSE       FALSE
#> 2          DENSITY_PCTL_N       scale     FALSE       FALSE
#> 3              MEDIAN_AGE         age     FALSE       FALSE
#> 4  SMALL_DENSITY_PER_100K        size      TRUE        TRUE
#> 5  LARGE_DENSITY_PER_100K        size      TRUE        TRUE
#> 6             SMALL_SHARE        size      TRUE        TRUE
#> 7             LARGE_SHARE        size      TRUE        TRUE
#> 8          DENSITY_PCTL_R       scale      TRUE        TRUE
#> 9                     HHI         hhi      TRUE        TRUE
#> 10                   NHHI         hhi      TRUE        TRUE
#> 11                    CR1          cr      TRUE        TRUE
#> 12                    CR2          cr      TRUE        TRUE
#> 13                    CR3          cr      TRUE        TRUE
#> 14                    CR4          cr      TRUE        TRUE
#> 15                    CR5          cr      TRUE        TRUE
#> 16                    CR6          cr      TRUE        TRUE
#> 17                    CR7          cr      TRUE        TRUE
#> 18                    CR8          cr      TRUE        TRUE
#> 19                  KWOKA       kwoka      TRUE        TRUE
#> 20                   GINI        gini      TRUE        TRUE
#> 21       COMMERCIAL_SHARE  commercial      TRUE       FALSE
#> 22  PROGRAM_REVENUE_SHARE  commercial      TRUE       FALSE
#> 23       FUNDRAISER_SHARE fundraising      TRUE       FALSE
#> 24      FUNDRAISING_RATIO fundraising      TRUE       FALSE
#>                       universes
#> 1  filers,registered,grassroots
#> 2  filers,registered,grassroots
#> 3  filers,registered,grassroots
#> 4                        filers
#> 5                        filers
#> 6                        filers
#> 7                        filers
#> 8                        filers
#> 9                        filers
#> 10                       filers
#> 11                       filers
#> 12                       filers
#> 13                       filers
#> 14                       filers
#> 15                       filers
#> 16                       filers
#> 17                       filers
#> 18                       filers
#> 19                       filers
#> 20                       filers
#> 21                       filers
#> 22                       filers
#> 23                       filers
#> 24                       filers
#>                                                                      description
#> 1                                            Organizations per 100,000 residents
#> 2  Organization count relative to the count expected for the market's population
#> 3                                         Median years since the IRS ruling year
#> 4           Small organizations (resource below small_max) per 100,000 residents
#> 5     Large organizations (resource at or above large_min) per 100,000 residents
#> 6                                          Share of organizations that are small
#> 7                                          Share of organizations that are large
#> 8      Resource total relative to the total expected for the market's population
#> 9                                  Herfindahl-Hirschman Index of resource shares
#> 10                         HHI normalized to 0-1 for the number of organizations
#> 11                      Share of the resource held by the 1 largest organization
#> 12                     Share of the resource held by the 2 largest organizations
#> 13                     Share of the resource held by the 3 largest organizations
#> 14                     Share of the resource held by the 4 largest organizations
#> 15                     Share of the resource held by the 5 largest organizations
#> 16                     Share of the resource held by the 6 largest organizations
#> 17                     Share of the resource held by the 7 largest organizations
#> 18                     Share of the resource held by the 8 largest organizations
#> 19                                                         Kwoka dominance index
#> 20                                              Gini coefficient of the resource
#> 21       Share of organizations with program revenue above half of total revenue
#> 22                           Program service revenue as a share of total revenue
#> 23                      Share of Form 990 filers paying professional fundraisers
#> 24                     Contributions per dollar of professional fundraising fees
npc_metrics( "grassroots" )
#>                 id family financial by_resource                    universes
#> 1 DENSITY_PER_100K  count     FALSE       FALSE filers,registered,grassroots
#> 2   DENSITY_PCTL_N  scale     FALSE       FALSE filers,registered,grassroots
#> 3       MEDIAN_AGE    age     FALSE       FALSE filers,registered,grassroots
#>                                                                     description
#> 1                                           Organizations per 100,000 residents
#> 2 Organization count relative to the count expected for the market's population
#> 3                                        Median years since the IRS ruling year
```
