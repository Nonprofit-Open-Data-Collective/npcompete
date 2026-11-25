# npcompete (development version)

## Documentation

* New README with a metrics overview, quick start, market definitions and
  the data included.
* Articles for each metric (HHI and NHHI, concentration ratios, Kwoka,
  Gini, scale-free density), each with descriptive statistics and plots by
  NTEE major group and metro size, plus an article on how the metrics
  correlate, with principal components and factor analysis.
* New dataset `cbsa_metrics`: every metric for every US metro and micro
  area in 2019, by subsector, on total revenue.
* Added this changelog.

## Local archive and multi-year builds (Phase 2)

* `npc_build_metrics()` builds every metric for any set of years, for CBSAs
  and counties and several resources at once, as a long table or as the
  wide tables of `get_all_metrics()`, optionally written to disk with a
  manifest of the build.
* `npc_build_archive()` converts the SOI panel sample frame CSV once into a
  Parquet archive partitioned by year; `npc_get_orgs()` reads only the
  years requested. `npc_standardize()`, `npc_archive_dir()` and
  `npc_archive_info()` support it. Requires the arrow package.
* New population data on fixed 2020 boundaries: `pop_county` and `pop_cbsa`
  (2000-2025) and the county-to-CBSA crosswalk `cbsa_county_xwalk` (2023
  delineation), read with `npc_get_population()`.
* Metric tables now include a national row (`GEO = "ALL"`, `SUB = "ALL"`).
* `check_year_completeness()` judges years against the typical count of
  substantial years, so a tail of nearly empty early years no longer hides
  incomplete ones.
* Faster rollups with data.table grouping.
* The live NCCS source and the registered and grassroots universes are
  planned; requesting them gives an informative error.

## Metric fixes and tests (Phase 1)

* All concentration metrics share one set of helpers and one API:
  `geography`, `subsector`, `resource` and `year` arguments, with
  `SUB = "ALL"` and `GEO = "ALL"` rollups.
* `get_scale_free_density()` now fits count models (negative binomial by
  default, or Poisson) on a full market frame that includes markets with no
  organizations, keeps national rows out of the models, and reports the
  density index as observed / expected - 1. The original log-linear model
  is available as `model = "loglinear"`.
* `get_kwoka()` uses the standard Kwoka (1977) index, the sum of squared
  gaps between successive shares.
* Single-organization markets get `NHHI = 1`, consistent with HHI, CR*k*
  and Kwoka.
* New `check_year_completeness()`; `get_all_metrics()` drops incomplete
  years by default.
* New dataset `npo_panel_sample` (Vermont 501(c)(3) filers, 2015-2019) for
  examples and tests.
* testthat suite with hand-computed values for every metric.

## Package cleanup (Phase 0)

* npcompete builds and passes `R CMD check`: scripts moved out of `R/`, one
  definition per function, complete `DESCRIPTION` and `NAMESPACE`.
* `get_density()`, `get_density_small()`, `get_density_big()`,
  `get_density_commercial()` and `get_fundraising_efficiency()` use the old
  NCCS core-file layout and are deprecated.
* pkgdown site with the NODC pkgstyle template; MIT license.

# npcompete 2022

* First versions of HHI, normalized HHI, CR1 to CR8, the Kwoka index, the
  Gini coefficient, per-capita density measures and fundraising efficiency
  on the NCCS core files, with the precomputed `data-share/np-compete.csv`
  (1990-2019) and a Shiny dashboard.
