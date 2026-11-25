# Kwoka Index

Calculate the Kwoka Index (kindex) and return a dataframe.

## Usage

``` r
get_kwoka_index(df, geo, subsector, resource)
```

## Arguments

- df:

  A dataframe containing the required fields for computing the
  metric(kwoka index) grouped by geographical level and sub sector.

- geo:

  A character string indicating the column name for georaphical area
  unique code.

- subsector:

  A character string indicating the column name for sub sectors.

- resource:

  A character string indicating the column name for resource based on
  which cr4 is calculated. Example: Revenue, assets etc.

## Value

A new dataframe with the Kwoka Index (`kindex`), grouped by geographical
level and sub sectors.

## Details

The Kwoka Index(kindex) is a measure of market concentration that is
used to understand market competitiveness, competitive behavior and
performance in markets.

## Examples

``` r
data(nonprofit_sample)
head( dat.kindex )
```
