# Professional fundraising in a market

Use of paid fundraisers and the contributions they bring in. Replaces
the legacy
[`get_fundraising_efficiency()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_fundraising_efficiency.md).

## Usage

``` r
get_fundraising_ratio(
  df,
  geography = "CENSUS_CBSA_FIPS",
  subsector = "NTEEV2",
  year = "TAXYEAR",
  contributions = "REVCONT",
  fees = "FNDFEE"
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

- contributions:

  Contributions and grants column.

- fees:

  Professional fundraising fees column.

## Value

A tibble with `GEO`, `SUB`, `YEAR`, `N_FEE` (organizations that report
the fee line), `FUNDRAISER_SHARE` (share of those paying professional
fundraisers) and `FUNDRAISING_RATIO` (contributions per dollar of
professional fundraising fees, among organizations that pay fees).

## Details

Professional fundraising fees are reported only on the full Form 990, so
Form 990-EZ filers are excluded, and only a few percent of filers pay
such fees. Markets with no fee-paying organizations get
`FUNDRAISING_RATIO = NA`. The fee line does not include in-house
fundraising staff, so this is not total fundraising efficiency.

## Examples

``` r
fr <- get_fundraising_ratio( npo_panel_sample, geography = "COUNTY_FIPS",
                             year = "TAXYEAR", fees = "FNDFEE" )
head( fr[ fr$SUB == "ALL", ] )
#> # A tibble: 6 × 6
#>   GEO   SUB    YEAR N_FEE FUNDRAISER_SHARE FUNDRAISING_RATIO
#>   <chr> <chr> <int> <int>            <dbl>             <dbl>
#> 1 50001 ALL    2015    71           0.0141            1488. 
#> 2 50001 ALL    2016    68           0.0147            8708. 
#> 3 50001 ALL    2017    71           0.0282             111. 
#> 4 50001 ALL    2018    73           0.0411              38.0
#> 5 50001 ALL    2019    75           0.0533              52.3
#> 6 50003 ALL    2015    85           0.0118              13.0
```
