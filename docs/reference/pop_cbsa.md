# CBSA population, 2000-2025

Annual population for core-based statistical areas (metropolitan and
micropolitan), computed by summing
[pop_county](https://nonprofit-open-data-collective.github.io/npcompete/reference/pop_county.md)
over the counties in each CBSA under the 2023 delineation, so every year
uses the same boundaries.

## Usage

``` r
pop_cbsa
```

## Format

A data frame with one row per CBSA and year:

- CBSA_FIPS:

  Five-digit CBSA code

- CBSA_NAME:

  CBSA title

- YEAR:

  Year

- POP:

  Population

## Source

U.S. Census Bureau; see
[pop_county](https://nonprofit-open-data-collective.github.io/npcompete/reference/pop_county.md).

## See also

[cbsa_county_xwalk](https://nonprofit-open-data-collective.github.io/npcompete/reference/cbsa_county_xwalk.md),
[`npc_get_population()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_get_population.md)
