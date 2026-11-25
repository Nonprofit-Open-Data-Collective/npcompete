# Concentration Ratio

Calculate the Concentration Ratio (CR) of top n(number) largest
organizations and return a dataframe.

## Usage

``` r
get_cr(df, geo, subsector, resource, number)
```

## Arguments

- df:

  A dataframe containing the required fields for computing the
  metric(concentration ratio) grouped by geographical level and sub
  sector.

- geo:

  A character string indicating the column name for geographical area
  unique code.

- subsector:

  A character string indicating the column name for sub sectors.

- resource:

  A character string indicating the column name for resource based on
  which cr is calculated. Example: Revenue, assets etc.

- number:

  A numeric value that is used to calculate concentration ratio for top
  that number of firms. Value ranges from 1-8. Example, 4 if
  concentration ratio for top 4 firms.

## Value

A new dataframe with the Concentration Ratio of n(number) largest
organizations ('cr'), grouped by geographical level and sub sectors
\*Note: For the markets where total number of firms is less than
n(umber), the concentration ratio is assigned 100%.

## Details

Concentration Ratio is defined as the summation of the market shares of
the largest n firms. It is calculated using the summation of revenue of
the largest n nonprofits divided by the summation of revenue of all
nonprofits in a market. The formula is \\\frac{\sigma{revnue of top-n
firms}}{Total resource of that subsector}\\

## Examples

``` r
data(nonprofit_sample)
head( dat.cr )
```
