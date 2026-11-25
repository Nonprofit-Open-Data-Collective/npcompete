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
- [`npc_compute_metrics()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_compute_metrics.md)
  : Compute a set of metrics from the registry
- [`npc_metrics()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_metrics.md)
  : List the available metrics
- [`npc_build_archive()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_build_archive.md)
  : Build the local Parquet archive from the SOI panel CSV
- [`npc_archive_dir()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_archive_dir.md)
  : Location of the local data archive
- [`npc_archive_info()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_archive_info.md)
  : Describe the local archive or a live NCCS build
- [`npc_get_orgs()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_get_orgs.md)
  : Read organization-years for a set of years
- [`npc_get_population()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_get_population.md)
  : Market populations for a geography level
- [`npc_standardize()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_standardize.md)
  : Convert an organization panel to the canonical npcompete schema

## Building from NCCS downloads

Download NCCS CORE and BMF files, harmonize them to the archive layout,
and check them against the archive.

- [`npc_build_nccs()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_build_nccs.md)
  : Build the harmonized organization table from NCCS downloads
- [`npc_cache_dir()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_cache_dir.md)
  : Location of the NCCS download cache
- [`npc_nccs_download()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_nccs_download.md)
  : Download a file from the NCCS data bucket
- [`npc_match_county()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_match_county.md)
  : Match county names to county FIPS codes
- [`npc_compare_sources()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_compare_sources.md)
  : Compare live NCCS filers with the local archive

## Registered and grassroots universes

Every organization on the IRS registry, including 990-N filers and
non-filers.

- [`npc_build_registered()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_build_registered.md)
  : Build the registered universe of organizations
- [`npc_registry_spans()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_registry_spans.md)
  : Registration spans from the BMF and the revocation list
- [`get_grassroots_share()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_grassroots_share.md)
  : Grassroots share of registered organizations

## Dynamics

Entry, exit, churn, graduation and downgrade rates by market.

- [`get_dynamics()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_dynamics.md)
  : Entry, exit and churn rates by market

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
- [`get_density_per_capita()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_density_per_capita.md)
  : Nonprofit density per capita, overall and by size
- [`get_density_small()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_density_small.md)
  : Density small

## Efficiency

- [`get_fundraising_efficiency()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_fundraising_efficiency.md)
  : Fundraising efficiency
- [`get_fundraising_ratio()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_fundraising_ratio.md)
  : Professional fundraising in a market
- [`get_commercial_share()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_commercial_share.md)
  : Commercial share of a market

## Utilities

- [`check_year_completeness()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/check_year_completeness.md)
  : Flag years with incomplete data
- [`get_median_age()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_median_age.md)
  : Median organization age

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
