# County population, 2000-2025

Annual population for U.S. counties on 2020 boundaries. Built from the
2000, 2010 and 2020 decennial censuses and the Census Bureau's vintage
2024 population estimates (2021-2024), with linear interpolation between
anchor years and 2025 carried forward from 2024. Built by
`data-raw/population/create-population-data.R`.

## Usage

``` r
pop_county
```

## Format

A data frame with one row per county and year:

- COUNTY_FIPS:

  Five-digit county FIPS code

- STATE_FIPS:

  Two-digit state FIPS code

- COUNTY_NAME:

  County name

- YEAR:

  Year

- POP:

  Population

## Source

U.S. Census Bureau decennial census and population estimates, via
`tidycensus`.

## See also

[pop_cbsa](https://nonprofit-open-data-collective.github.io/npcompete/reference/pop_cbsa.md),
[cbsa_county_xwalk](https://nonprofit-open-data-collective.github.io/npcompete/reference/cbsa_county_xwalk.md),
[`npc_get_population()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_get_population.md)
