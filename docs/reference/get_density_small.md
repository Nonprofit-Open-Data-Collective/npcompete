# Density small

Calculate the nonprofit density metric having resource smaller than a
numeric value and return a dataframe.

## Usage

``` r
get_density_small(df, geo, subsector, fips, year, resource, smaller.than)
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

- resource:

  A character string indicating the column name for resource based on
  which density is evaluated

- smaller.than:

  A numeric value indicating that it is the upper limit of the resource
  for density evaluation. All organizations having resource value less
  than that would be counted for evaluation.

## Value

A new dataframe with the density of nonprofits per 100000 population,
having resource smaller than the given number grouped by geographical
level and sub sectors.

## Details

The density metric is the number of nonprofits per-capita in a market.
In this function, it is measured as the number of organizations per
100,000 population.

## Examples

``` r
data(county_population)
data(nonprofit_sample)
dat.den.2009 <- get_density_small( df=nonprofit_sample,'MSA_NECH','NTMAJ12','FIPS', 2009, 'TOTREV', 100000)
head( dat.den.2009 )
```
