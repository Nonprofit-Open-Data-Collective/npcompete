# Package index

## All metrics at once

Compute every competition metric for each market in one call.

- [`get_all_metrics()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_all_metrics.md)
  : Compute all competition metrics in one table

## Building from the local archive

Convert the SOI panel to a Parquet archive and build metrics for any set
of years.

- [`npc_build_metrics()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_build_metrics.md)
  : Build competition and density metrics for any set of years
- [`npc_build_archive()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_build_archive.md)
  : Build the local Parquet archive from the SOI panel CSV
- [`npc_archive_dir()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_archive_dir.md)
  : Location of the local data archive
- [`npc_archive_info()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_archive_info.md)
  : Describe the local archive
- [`npc_get_orgs()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_get_orgs.md)
  : Read organization-years for a set of years
- [`npc_get_population()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_get_population.md)
  : Market populations for a geography level
- [`npc_standardize()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_standardize.md)
  : Convert an organization panel to the canonical npcompete schema

## Concentration

How resources are shared among the organizations in a market.

- [`get_hhi()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_hhi.md)
  : Herfindahl–Hirschman Index
- [`get_nhhi()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_nhhi.md)
  : Normalized Herfindahl–Hirschman Index
- [`get_cr()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_cr.md)
  : Concentration ratio (CRk)
- [`get_cr_all()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_cr_all.md)
  : Concentration ratios CR1 to CRk
- [`get_kwoka()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_kwoka.md)
  : Kwoka dominance index
- [`get_gini()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_gini.md)
  : Gini coefficient

## Density

Nonprofits per capita, overall and by size or type.

- [`get_scale_free_density()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_scale_free_density.md)
  : Scale-free nonprofit density
- [`get_density()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_density.md)
  : Density metric
- [`get_density_big()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_density_big.md)
  : Density big
- [`get_density_commercial()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_density_commercial.md)
  : Density commercial
- [`get_density_small()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_density_small.md)
  : Density small

## Efficiency

- [`get_fundraising_efficiency()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_fundraising_efficiency.md)
  : Fundraising efficiency

## Utilities

- [`check_year_completeness()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/check_year_completeness.md)
  : Flag years with incomplete data

## Data

Sample organizations and population counts used by the examples.

- [`npo_panel_sample`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npo_panel_sample.md)
  : Sample of the SOI panel sample frame (Vermont, 2015-2019)
- [`cbsa_metrics`](https://nonprofit-open-data-collective.github.io/npcompete/reference/cbsa_metrics.md)
  : Competition metrics for every US metro and micro area, 2019
- [`nonprofit_sample`](https://nonprofit-open-data-collective.github.io/npcompete/reference/nonprofit_sample.md)
  : Sample data from NCCS data archive
- [`county_population`](https://nonprofit-open-data-collective.github.io/npcompete/reference/county_population.md)
  : County wise population with FIPS
- [`pop_county`](https://nonprofit-open-data-collective.github.io/npcompete/reference/pop_county.md)
  : County population, 2000-2025
- [`pop_cbsa`](https://nonprofit-open-data-collective.github.io/npcompete/reference/pop_cbsa.md)
  : CBSA population, 2000-2025
- [`cbsa_county_xwalk`](https://nonprofit-open-data-collective.github.io/npcompete/reference/cbsa_county_xwalk.md)
  : County to CBSA crosswalk (2023 delineation)
