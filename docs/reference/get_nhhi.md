# Normalized Herfindahl–Hirschman Index

Calculate the normalized Herfindahl–Hirschman Index (HHI) and return a
dataframe.

## Usage

``` r
get_nhhi(df, geo, subsector, resource, normalizer, resource.name = "resource")
```

## Arguments

- df:

  A dataframe containing the required fields for computing the
  metric(hhi) grouped by geographical level and sub sector.

- geo:

  A character string indicating the column name for geographical area
  unique code.

- subsector:

  A character string indicating the column name for sub sectors.

- resource:

  A character string indicating the column name for resource based on
  which HHI is calculated. Example: Revenue, assets etc.

- normalizer:

  A character string indicating the column name for the column used to
  normalize the HHI index.

- resource.name:

  A character string indicating the column name for the new column
  generated with aggregated resource.

## Value

A new dataframe with the normalized Herfindahl–Hirschman Index (`hhi`),
grouped by geographical level and sub sectors.

## Details

The Herfindahl–Hirschman Index(HHI) is a measure of market concentration
that is used to understand market competitiveness, competitive behavior
and performance in markets. The index could be calculated as the sum of
the squared market share of each firm competing in a market (Thornton &
Belski, 2010).In the nonprofit sector, market share is determined by the
ratio of a nonprofit's total revenue to the aggregate revenue for the
organization’s market.The formula used here is \\{\sigma
\frac{resource}{Total resource}}^2 -
\frac{\frac{1}{normalizer}}{1-\frac{1}{normalizer}}\\ The calculated
values range from 1/N to 1, where N is the number of organizations in a
market. A market with an HHI of 1 is considered as a monopoly. In
contrast, a market with an HHI close to 0 would be viewed as a nearly
competition one.

## Examples

``` r
data(nonprofit_sample)
head( dat.nhhi )
```
