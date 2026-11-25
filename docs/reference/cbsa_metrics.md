# Competition metrics for every US metro and micro area, 2019

Output of
[`get_all_metrics()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_all_metrics.md)
for every core-based statistical area (CBSA) in the SOI panel sample
frame, tax year 2019, 501(c)(3) filers only, measured on total revenue.
One row per CBSA and NTEE major group, plus one `SUB = "ALL"` row per
CBSA. National rollups are left out. Used by the vignettes for
cross-market comparisons; built by `data-raw/create-cbsa-metrics.R`.

## Usage

``` r
cbsa_metrics
```

## Format

A data frame with 11,938 rows and 22 columns:

- GEO:

  CBSA code

- SUB:

  NTEE major group (the first three characters of `NTEEV2`, the NCCS
  equivalent of the 12 NTMAJ12 groups), or `"ALL"`

- YEAR:

  Tax year (2019)

- N:

  Number of organizations; `0` for subsectors with no organizations in
  that CBSA

- POP:

  CBSA population; `NA` for a few CBSAs missing from the population
  table

- TOTREV:

  Summed total revenue

- EXPECTED_N, EXPECTED_TOTREV:

  Count and revenue expected for a market of this population, from the
  density models

- DENSITY_PCTL_N, DENSITY_PCTL_R:

  Scale-free density of the count and of revenue, from
  [`get_scale_free_density()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_scale_free_density.md)

- GINI, HHI, NHHI, KWOKA:

  Gini coefficient, Herfindahl-Hirschman index, normalized HHI and Kwoka
  dominance index; `NA` when `N = 0`

- CR1, CR2, CR3, CR4, CR5, CR6, CR7, CR8:

  Concentration ratios

## Source

NCCS SOI panel sample frame, <https://nccs.urban.org/>
