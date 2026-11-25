# Gini Coefficient

Calculate the Gini Coefficient and return a dataframe.

## Usage

``` r
get_gini(df, geo, subsector, resource)
```

## Arguments

- df:

  A dataframe containing the required fields for computing the
  metric(gini) grouped by geographical level and sub sector.

- geo:

  A character string indicating the column name for geographical area
  unique code.

- subsector:

  A character string indicating the column name for sub sectors.

- resource:

  A character string indicating the column name for resource based on
  which gini is calculated. Example: Revenue, assets etc.

- resource.name:

  A character string indicating the column name for the new column
  generated with aggregated resource.

- fips:

  A character string indicating the column name for FIPS code of the
  geographical area

- year:

  A numeric parameter indicating the year to be used for population
  estimate and metric evaluation for that year. Example: 1999

- resource.smaller.than:

  A numeric value indicating that it is the upper limit of the resource
  for density evaluation. All organizations having resource value less
  than that would be counted for evaluation.

## Value

A new dataframe with the Gini Coefficient (`gini`), grouped by
geographical level and sub sectors.

## Details

The Gini coefficient has been widely used in economics as a measure of
income inequality. Specifically, it measures income distribution among a
population. In economics, there are multiple formulas used to calculate
the coefficient (Dorfman, 1979). However, no matter which formula is
selected, a low value of the coefficient indicates a high level of
competition (less inequality); a high value represents a low level of
competition (greater inequality). In nonprofit study, Seaman et al.,
(2014) supplemented HHI with the Gini coefficient to capture the concept
of market competition and suggested that a market is considered as a
reasonable level of equality if the coefficient is less than 0.5; a
market is considered as a modest level of inequality if the coefficient
is between 0.5 and 0.75; a market is considered as a high level of
inequality if the coefficient is greater than .75.

## Examples

``` r
data(nonprofit_sample)
head( dat.gini )
```
