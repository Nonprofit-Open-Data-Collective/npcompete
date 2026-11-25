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
`default` (included in `metrics = "default"`; dynamics metrics are not,
since they read extra years), `universes` and `description`.

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
#> 4        GRASSROOTS_SHARE  grassroots     FALSE       FALSE
#> 5  SMALL_DENSITY_PER_100K        size      TRUE        TRUE
#> 6  LARGE_DENSITY_PER_100K        size      TRUE        TRUE
#> 7             SMALL_SHARE        size      TRUE        TRUE
#> 8             LARGE_SHARE        size      TRUE        TRUE
#> 9          DENSITY_PCTL_R       scale      TRUE        TRUE
#> 10                    HHI         hhi      TRUE        TRUE
#> 11                   NHHI         hhi      TRUE        TRUE
#> 12                    CR1          cr      TRUE        TRUE
#> 13                    CR2          cr      TRUE        TRUE
#> 14                    CR3          cr      TRUE        TRUE
#> 15                    CR4          cr      TRUE        TRUE
#> 16                    CR5          cr      TRUE        TRUE
#> 17                    CR6          cr      TRUE        TRUE
#> 18                    CR7          cr      TRUE        TRUE
#> 19                    CR8          cr      TRUE        TRUE
#> 20                  KWOKA       kwoka      TRUE        TRUE
#> 21                   GINI        gini      TRUE        TRUE
#> 22       COMMERCIAL_SHARE  commercial      TRUE       FALSE
#> 23  PROGRAM_REVENUE_SHARE  commercial      TRUE       FALSE
#> 24       FUNDRAISER_SHARE fundraising      TRUE       FALSE
#> 25      FUNDRAISING_RATIO fundraising      TRUE       FALSE
#> 26             ENTRY_RATE    dynamics     FALSE       FALSE
#> 27              EXIT_RATE    dynamics     FALSE       FALSE
#> 28             NET_GROWTH    dynamics     FALSE       FALSE
#> 29             CHURN_RATE    dynamics     FALSE       FALSE
#> 30        GRADUATION_RATE    dynamics     FALSE       FALSE
#> 31         DOWNGRADE_RATE    dynamics     FALSE       FALSE
#>                       universes default
#> 1  filers,registered,grassroots    TRUE
#> 2  filers,registered,grassroots    TRUE
#> 3  filers,registered,grassroots    TRUE
#> 4                    registered    TRUE
#> 5                        filers    TRUE
#> 6                        filers    TRUE
#> 7                        filers    TRUE
#> 8                        filers    TRUE
#> 9                        filers    TRUE
#> 10                       filers    TRUE
#> 11                       filers    TRUE
#> 12                       filers    TRUE
#> 13                       filers    TRUE
#> 14                       filers    TRUE
#> 15                       filers    TRUE
#> 16                       filers    TRUE
#> 17                       filers    TRUE
#> 18                       filers    TRUE
#> 19                       filers    TRUE
#> 20                       filers    TRUE
#> 21                       filers    TRUE
#> 22                       filers    TRUE
#> 23                       filers    TRUE
#> 24                       filers    TRUE
#> 25                       filers    TRUE
#> 26 filers,registered,grassroots   FALSE
#> 27 filers,registered,grassroots   FALSE
#> 28 filers,registered,grassroots   FALSE
#> 29 filers,registered,grassroots   FALSE
#> 30                   registered   FALSE
#> 31                   registered   FALSE
#>                                                                      description
#> 1                                            Organizations per 100,000 residents
#> 2  Organization count relative to the count expected for the market's population
#> 3                                         Median years since the IRS ruling year
#> 4             Share of registered organizations that file a 990-N or do not file
#> 5           Small organizations (resource below small_max) per 100,000 residents
#> 6     Large organizations (resource at or above large_min) per 100,000 residents
#> 7                                          Share of organizations that are small
#> 8                                          Share of organizations that are large
#> 9      Resource total relative to the total expected for the market's population
#> 10                                 Herfindahl-Hirschman Index of resource shares
#> 11                         HHI normalized to 0-1 for the number of organizations
#> 12                      Share of the resource held by the 1 largest organization
#> 13                     Share of the resource held by the 2 largest organizations
#> 14                     Share of the resource held by the 3 largest organizations
#> 15                     Share of the resource held by the 4 largest organizations
#> 16                     Share of the resource held by the 5 largest organizations
#> 17                     Share of the resource held by the 6 largest organizations
#> 18                     Share of the resource held by the 7 largest organizations
#> 19                     Share of the resource held by the 8 largest organizations
#> 20                                                         Kwoka dominance index
#> 21                                              Gini coefficient of the resource
#> 22       Share of organizations with program revenue above half of total revenue
#> 23                           Program service revenue as a share of total revenue
#> 24                      Share of Form 990 filers paying professional fundraisers
#> 25                     Contributions per dollar of professional fundraising fees
#> 26                         Organizations entering in t per organization in t - 1
#> 27                   Organizations exiting after t - 1 per organization in t - 1
#> 28                 Change in the number of organizations since t - 1, as a share
#> 29                                  Entries plus exits per organization in t - 1
#> 30  Share of 990-N filers and non-filers in t - 1 that file a 990 or 990-EZ in t
#> 31 Share of 990 and 990-EZ filers in t - 1 that file a 990-N or do not file in t
npc_metrics( "grassroots" )
#>                 id   family financial by_resource                    universes
#> 1 DENSITY_PER_100K    count     FALSE       FALSE filers,registered,grassroots
#> 2   DENSITY_PCTL_N    scale     FALSE       FALSE filers,registered,grassroots
#> 3       MEDIAN_AGE      age     FALSE       FALSE filers,registered,grassroots
#> 4       ENTRY_RATE dynamics     FALSE       FALSE filers,registered,grassroots
#> 5        EXIT_RATE dynamics     FALSE       FALSE filers,registered,grassroots
#> 6       NET_GROWTH dynamics     FALSE       FALSE filers,registered,grassroots
#> 7       CHURN_RATE dynamics     FALSE       FALSE filers,registered,grassroots
#>   default
#> 1    TRUE
#> 2    TRUE
#> 3    TRUE
#> 4   FALSE
#> 5   FALSE
#> 6   FALSE
#> 7   FALSE
#>                                                                     description
#> 1                                           Organizations per 100,000 residents
#> 2 Organization count relative to the count expected for the market's population
#> 3                                        Median years since the IRS ruling year
#> 4                         Organizations entering in t per organization in t - 1
#> 5                   Organizations exiting after t - 1 per organization in t - 1
#> 6                 Change in the number of organizations since t - 1, as a share
#> 7                                  Entries plus exits per organization in t - 1
```
