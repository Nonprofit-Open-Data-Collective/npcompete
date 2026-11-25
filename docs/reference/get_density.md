# Density metric

Calculate the nonprofit density metric and return a dataframe.

## Usage

``` r
get_density(df, geo, subsector, fips, year)
```

## Arguments

- df:

  A dataframe containing the required fields for computing the
  metric(density) grouped by gepgraphical level and sub sector.

- geo:

  A character string indicating the column name for geographical area
  unique code.

- subsector:

  A character string indicating the column name for sub sectors.

- fips:

  A character string indicating the column name for FIPS code of the
  geographical area

- year:

  A numeric parameter indicating the year to be used for population
  estimate. Example: 1999

## Value

A new dataframe with the density of nonprofits per 100000 population,
grouped by geographical level and sub sectors.

## Details

The density metric is the number of nonprofits per-capita in a market.
In this function, it is measured as the number of organizations per
100,000 population.

## Deprecated

This function uses the legacy NCCS core-file layout and is deprecated.

## Examples

``` r
if (FALSE) { # \dontrun{
get_density( nonprofit_sample, 'MSA_NECH', 'NTMAJ12', 'FIPS', 2009 )
} # }
```
