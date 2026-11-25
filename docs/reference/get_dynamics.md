# Entry, exit and churn rates by market

Follows each organization across years and counts, for every market and
year `t`, the organizations that entered (present in `t`, not in
`t - 1`) and exited (present in `t - 1`, then absent for at least
`absent_years` years). Rates use the stock in `t - 1` as the
denominator, with the market and subsector the organization had in
`t - 1` for exits and in `t` for entries. Organizations that move
between markets count as neither.

## Usage

``` r
get_dynamics(
  df,
  geography = "CBSA_FIPS",
  subsector = "NTEEV2",
  year = "YEAR",
  id = "EIN2",
  filer_type = NULL,
  revocation = "REVOCATION_YEAR",
  absent_years = 2,
  observed_years = NULL,
  drop_ppa_revocations = FALSE
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

- id:

  Name of the organization id column.

- filer_type:

  Optional filer type column (`"990"`, `"990EZ"`, `"990N"`,
  `"NONFILER"`, ...). When given, graduation and downgrade rates are
  added (see Value).

- revocation:

  Name of the revocation year column, if any; used for `EXITS_PPA`.

- absent_years:

  Years of absence that confirm an exit. Use 2 or more for filers, whose
  filings can lapse a year; 1 for the registered universe, where absence
  is observed directly.

- observed_years:

  The years `df` covers, which must be consecutive. Defaults to the
  years present in `df`. Pass it when some observed years are not
  reported, so that their absences still count.

- drop_ppa_revocations:

  Leave revocation exits of 2010-2012 out of `EXITS` and the rates.

## Value

A tibble with `GEO`, `SUB`, `YEAR` and:

- N_PREV, N_CUR:

  Organizations in `t - 1` and in `t`

- ENTRIES, EXITS, EXITS_PPA:

  Counts; `EXITS_PPA` are exits in 2010-2012 that coincide with a
  revocation

- ENTRY_RATE, EXIT_RATE:

  `ENTRIES / N_PREV`, `EXITS / N_PREV`

- NET_GROWTH:

  `(N_CUR - N_PREV) / N_PREV`, which also reflects moves between markets

- CHURN_RATE:

  `(ENTRIES + EXITS) / N_PREV`

- N_PREV_GRASSROOTS, GRADUATIONS, GRADUATION_RATE:

  With `filer_type`: 990-N filers and non-filers in `t - 1`, how many
  filed a 990 or 990-EZ in `t`, and the share

- N_PREV_FILERS, DOWNGRADES, DOWNGRADE_RATE:

  With `filer_type`: 990 and 990-EZ filers in `t - 1`, how many were
  990-N filers or non-filers in `t`, and the share

Rates are `NA` where `N_PREV` is 0.

## What an exit means

In the filers universe an exit is an **exit from filing**: an
organization that stops filing a Form 990 or 990-EZ may have dropped
below the filing threshold and switched to the 990-N postcard, so an
exit is not a death. In the registered universe an exit is removal from
the IRS registry: revocation, termination or merger.

## Gaps and censoring

Absences shorter than `absent_years` between two appearances are filled:
the organization counts as present, so a late or missed filing is
neither an exit nor a re-entry. A longer absence ends with a re-entry.
Exits in the last `absent_years - 1` observed years cannot be confirmed
yet and are reported as `NA`, as are all rates for the first observed
year, which has no prior year.

## Mass revocations of 2010-2012

The Pension Protection Act revoked the exemptions of hundreds of
thousands of organizations that had not filed for three years, almost
all of them in 2010-2012. `EXITS_PPA` counts exits in those years that
coincide with a revocation. `drop_ppa_revocations = TRUE` leaves them
out of `EXITS` and the rates.

## Examples

``` r
d <- data.frame(
  EIN2 = c( "a", "a", "a", "a", "b", "b", "c", "c", "d", "d" ),
  GEO  = "X", SUB = "ART",
  YEAR = c( 2018:2021, 2018, 2019, 2020, 2021, 2018, 2020 ) )
# a: present throughout; b: exits after 2019; c: enters in 2020;
# d: misses 2019 (filled as present with absent_years = 2), then gone
# after 2020, which cannot be confirmed as an exit until 2022
get_dynamics( d, geography = "GEO", subsector = "SUB", year = "YEAR",
              absent_years = 2 )
#> # A tibble: 12 × 12
#>    GEO   SUB    YEAR N_PREV N_CUR ENTRIES EXITS EXITS_PPA ENTRY_RATE EXIT_RATE
#>    <chr> <chr> <int>  <dbl> <dbl>   <dbl> <dbl>     <dbl>      <dbl>     <dbl>
#>  1 ALL   ALL    2019      3     3       0     0         0      0         0    
#>  2 ALL   ALL    2020      3     3       1     1         0      0.333     0.333
#>  3 ALL   ALL    2021      3     2       0    NA        NA      0        NA    
#>  4 ALL   ART    2019      3     3       0     0         0      0         0    
#>  5 ALL   ART    2020      3     3       1     1         0      0.333     0.333
#>  6 ALL   ART    2021      3     2       0    NA        NA      0        NA    
#>  7 X     ALL    2019      3     3       0     0         0      0         0    
#>  8 X     ALL    2020      3     3       1     1         0      0.333     0.333
#>  9 X     ALL    2021      3     2       0    NA        NA      0        NA    
#> 10 X     ART    2019      3     3       0     0         0      0         0    
#> 11 X     ART    2020      3     3       1     1         0      0.333     0.333
#> 12 X     ART    2021      3     2       0    NA        NA      0        NA    
#> # ℹ 2 more variables: NET_GROWTH <dbl>, CHURN_RATE <dbl>
```
