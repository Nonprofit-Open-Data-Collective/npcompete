# County to CBSA crosswalk (2023 delineation)

Counties that belong to a metropolitan or micropolitan statistical area
under the Census Bureau's July 2023 delineation. Counties outside any
CBSA are not listed.

## Usage

``` r
cbsa_county_xwalk
```

## Format

A data frame with one row per county:

- COUNTY_FIPS:

  Five-digit county FIPS code

- COUNTY_NAME, STATE_NAME:

  County and state names

- CBSA_FIPS, CBSA_NAME:

  CBSA code and title

- CBSA_TYPE:

  `"Metropolitan"` or `"Micropolitan"`

- CSA_FIPS, CSA_NAME:

  Combined statistical area, if any

- CENTRAL_OUTLYING:

  Whether the county is a central or outlying county

- DELINEATION:

  Delineation vintage (2023)

## Source

Census Bureau delineation file `list1_2023.xlsx`, as distributed by NBER
(`cbsa2fipsxw`).
