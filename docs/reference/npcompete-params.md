# Shared arguments for npcompete metric functions

Shared arguments for npcompete metric functions

## Arguments

- df:

  A data frame with one row per organization-year.

- geography:

  Name of the column that identifies the market's geography, such as
  `"CENSUS_CBSA_FIPS"` or `"COUNTY_FIPS"`.

- subsector:

  Name of the column that identifies the subsector. When it is
  `"NTEEV2"`, the first three characters (the major group) are used.

- resource:

  Name of the numeric column that measures organization size, such as
  `"TOTREV"`, `"TOTEXP"` or `"NETASS"`. Negative values are set to zero.

- year:

  Name of the column that identifies the year, such as `"TAXYEAR"`.

- population:

  Name of the column that holds the market's population, such as
  `"POP_METRO"` or `"POP_COUNTY"`. Ignored, and may be `NULL`, when
  `pop_table` is supplied.
