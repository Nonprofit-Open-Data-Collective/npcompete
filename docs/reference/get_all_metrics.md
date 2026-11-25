# Get all metrics

Calculate all competition metrics and return in a single dataframe and
return a dataframe.

## Usage

``` r
get_all_metrics(
  df,
  geo,
  subsector,
  resource,
  resource.name,
  normalizer,
  fips,
  dat.year,
  resource.smaller.than,
  resource.greater.than,
  fundraising.cols
)
```

## Arguments

- df:

  A dataframe containing the required fields for computing the metric
  grouped by geographical level and sub sector.

- geo:

  A character string indicating the column name for geographical area
  unique code.

- subsector:

  A character string indicating the column name for sub sectors.

- resource:

  A character string indicating the column name for resource based on
  which HHI is calculated. Example: Revenue, assets etc.

- resource.name:

  A character string indicating the column name for the new column
  generated with aggregated resource.

- normalizer:

  A character string indicating the column name for the column used to
  normalize the HHI index.

- resource.greater.than:

  A numeric value indicating that it is the lower limit of the resource
  for density evaluation. All organizations having resource value
  greater than that would be counted for evaluation.

- fundraising.cols:

  A vector containing list of column names that have the fundraising
  fees for the nonprofits.#'

- number:

  A numeric value that is used to calculate concentration ratio for top
  that number of firms. Value ranges from 1-8. Example, 4 if
  concentration ratio for top 4 firms.

- smaller.than:

  A numeric value indicating that it is the upper limit of the resource
  for density evaluation. All organizations having resource value less
  than that would be counted for evaluation.

## Value

A new dataframe with all metrics evaluated for each year.

## Examples

``` r
head( dat.all )
```
