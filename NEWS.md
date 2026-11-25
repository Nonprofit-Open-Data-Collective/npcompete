# npcompete (development version)

## Registered and grassroots universes (Phase 5, in progress)

* `npc_get_orgs()` and `npc_build_metrics()` accept `universe =
  "registered"` (every organization on the IRS registry that year) and
  `"grassroots"` (registered organizations that file a 990-N or do not
  file). `npc_build_registered()` builds them from the unified BMF, the IRS
  automatic revocation list and the 990-N e-postcard file, with filers from
  the archive or a live NCCS build. `npc_registry_spans()` gives each
  organization's registration years.
* Each registered organization-year has a `FILER_TYPE`: `"990"`, `"990EZ"`,
  `"990PF"` (private foundations), `"990N"` (an e-postcard for that year,
  or a BMF filing requirement that allows the 990-N) or `"NONFILER"`. New
  `CHURCH`, `FOUNDATION` and `FILING_REQ_CODE` columns; churches are kept
  unless `exclude_churches = TRUE`.
* New metric `GRASSROOTS_SHARE` (`get_grassroots_share()`), for the
  registered universe.

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

## Live NCCS source (Phase 4)

* New articles: "Building metrics from an archived panel" and "Building
  metrics from fresh NCCS downloads".

* `source = "nccs"` in `npc_get_orgs()` and `npc_build_metrics()` builds
  from NCCS downloads instead of the local archive. `npc_build_nccs()`
  downloads the CORE 990combined files and the unified geocoded BMF,
  harmonizes them to the archive's layout (`npc_standardize(source =
  "nccs_core")`) and caches them under `npc_cache_dir()`. Missing years are
  built on demand.
* Tax years follow the SOI convention (the year the fiscal period began),
  so tax year *t* combines CORE files *t* and *t* + 1. A year whose *t* + 1
  file is still partial is flagged incomplete.
* `npc_nccs_download()` caches files with retries and logs URL, size and MD5
  in `downloads.csv`; `year-counts.csv` and `manifest.csv` record the CORE
  files and BMF used for each year.
* `npc_match_county()` matches geocoded county names to FIPS codes,
  including parishes, boroughs and independent cities.
* `npc_compare_sources()` compares a live build with the archive. For 2018
  and 2019 the EIN sets agree to within 4 organizations a year, and
  revenue, expenses and assets agree for virtually every organization (net
  assets for 99.9%).
* The archive now has one row per EIN and tax year. The SOI panel repeats
  about 0.3% of EIN-years (short-period returns after an accounting-period
  change, and repeats across extracts), which counted those organizations
  twice. `npc_standardize()` and `npc_get_orgs()` keep a real filing over an
  imputed row, then the latest source file, then the latest period end, the
  same filing the NCCS files keep. Existing archives are deduplicated when
  read; rebuilding is not needed.
* Live builds have no fundraising fees, revocation fields or geocoded CBSA;
  those columns are `NA`, and CBSAs come from the county crosswalk.

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
