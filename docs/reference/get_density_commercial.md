# Density commercial

Calculate the commercial nonprofit density a resource and return a
dataframe.

## Usage

``` r
get_density_commercial(df, geo, subsector, resource, list.of.cols)
```

## Arguments

- df:

  A dataframe containing the required fields for computing the
  metric(density commercial) grouped by geographical level and sub
  sector.

- geo:

  A character string indicating the column name for geographical area
  unique code.

- subsector:

  A character string indicating the column name for sub sectors.

- resource:

  A character string indicating the column name for resource based on
  which density is evaluated

- list.of.cols:

  A vector containing list of column names that have the commercial
  metrics for the resource

## Value

A new dataframe with the commercial density of nonprofits summarized in
the subsector and geo level.

## Details

The density metric is the number of nonprofits per-capita in a market.
In this function, it is measured as the number of organizations per 1000
population and per 10,000 population.

## Examples

``` r
data(county_population)
data(nonprofit_sample)
dat.den.comm <- get_density_commercial( df=nonprofit_sample,'MSA_NECH','NTMAJ12', 'TOTREV', c('PROGREV', 'INVINC'))
head( dat.den.comm )
```
