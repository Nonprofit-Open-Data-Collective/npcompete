# Changelog

## npcompete (development version)

### Registered and grassroots universes (Phase 5, in progress)

- [`npc_get_orgs()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_get_orgs.md)
  and
  [`npc_build_metrics()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_build_metrics.md)
  accept `universe = "registered"` (every organization on the IRS
  registry that year) and `"grassroots"` (registered organizations that
  file a 990-N or do not file).
  [`npc_build_registered()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_build_registered.md)
  builds them from the unified BMF, the IRS automatic revocation list
  and the 990-N e-postcard file, with filers from the archive or a live
  NCCS build.
  [`npc_registry_spans()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_registry_spans.md)
  gives each organization’s registration years.
- Each registered organization-year has a `FILER_TYPE`: `"990"`,
  `"990EZ"`, `"990PF"` (private foundations), `"990N"` (an e-postcard
  for that year, or a BMF filing requirement that allows the 990-N) or
  `"NONFILER"`. New `CHURCH`, `FOUNDATION` and `FILING_REQ_CODE`
  columns; churches are kept unless `exclude_churches = TRUE`.
- New metric `GRASSROOTS_SHARE`
  ([`get_grassroots_share()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_grassroots_share.md)),
  for the registered universe.
- Dynamics:
  [`get_dynamics()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_dynamics.md)
  and the metrics `ENTRY_RATE`, `EXIT_RATE`, `NET_GROWTH`, `CHURN_RATE`
  (every universe) and `GRADUATION_RATE`, `DOWNGRADE_RATE` (registered).
  Rates use the stock in *t* - 1; absences shorter than `absent_years`
  (2 for filers, 1 for registered) are filled; exits that cannot be
  confirmed yet are `NA`. Exits in 2010-2012 that coincide with an
  automatic revocation are counted in `EXITS_PPA` and can be dropped
  with `drop_ppa_revocations = TRUE`.
  [`npc_build_metrics()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_build_metrics.md)
  reads the extra years these need. Dynamics metrics are not part of
  `metrics = "default"`; use `"all"` or name them.
- [`npc_metrics()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_metrics.md)
  gains a `default` column.
- New article “Universes, entries and exits”: how the universes and
  dynamics are built, and national, subsector and metro patterns of
  entry, exit and graduation for 501(c)(3)s.
- Live builds read the legacy NCCS CORE files for tax years 2010-2011,
  which have no form type: `FILER_TYPE` is inferred from the 990-EZ
  limits there, and a message notes their lower coverage.

### Documentation

- New README with a metrics overview, quick start, market definitions
  and the data included.
- Articles for each metric (HHI and NHHI, concentration ratios, Kwoka,
  Gini, scale-free density), each with descriptive statistics and plots
  by NTEE major group and metro size, plus an article on how the metrics
  correlate, with principal components and factor analysis.
- New dataset `cbsa_metrics`: every metric for every US metro and micro
  area in 2019, by subsector, on total revenue.
- Added this changelog.

### Live NCCS source (Phase 4)

- New articles: “Building metrics from an archived panel” and “Building
  metrics from fresh NCCS downloads”.

- `source = "nccs"` in
  [`npc_get_orgs()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_get_orgs.md)
  and
  [`npc_build_metrics()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_build_metrics.md)
  builds from NCCS downloads instead of the local archive.
  [`npc_build_nccs()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_build_nccs.md)
  downloads the CORE 990combined files and the unified geocoded BMF,
  harmonizes them to the archive’s layout
  (`npc_standardize(source = "nccs_core")`) and caches them under
  [`npc_cache_dir()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_cache_dir.md).
  Missing years are built on demand.

- Tax years follow the SOI convention (the year the fiscal period
  began), so tax year *t* combines CORE files *t* and *t* + 1. A year
  whose *t* + 1 file is still partial is flagged incomplete.

- [`npc_nccs_download()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_nccs_download.md)
  caches files with retries and logs URL, size and MD5 in
  `downloads.csv`; `year-counts.csv` and `manifest.csv` record the CORE
  files and BMF used for each year.

- [`npc_match_county()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_match_county.md)
  matches geocoded county names to FIPS codes, including parishes,
  boroughs and independent cities.

- [`npc_compare_sources()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_compare_sources.md)
  compares a live build with the archive. For 2018 and 2019 the EIN sets
  agree to within 4 organizations a year, and revenue, expenses and
  assets agree for virtually every organization (net assets for 99.9%).

- The archive now has one row per EIN and tax year. The SOI panel
  repeats about 0.3% of EIN-years (short-period returns after an
  accounting-period change, and repeats across extracts), which counted
  those organizations twice.
  [`npc_standardize()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_standardize.md)
  and
  [`npc_get_orgs()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_get_orgs.md)
  keep a real filing over an imputed row, then the latest source file,
  then the latest period end, the same filing the NCCS files keep.
  Existing archives are deduplicated when read; rebuilding is not
  needed.

- Live builds have no fundraising fees, revocation fields or geocoded
  CBSA; those columns are `NA`, and CBSAs come from the county
  crosswalk.

### Local archive and multi-year builds (Phase 2)

- [`npc_build_metrics()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_build_metrics.md)
  builds every metric for any set of years, for CBSAs and counties and
  several resources at once, as a long table or as the wide tables of
  [`get_all_metrics()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_all_metrics.md),
  optionally written to disk with a manifest of the build.
- [`npc_build_archive()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_build_archive.md)
  converts the SOI panel sample frame CSV once into a Parquet archive
  partitioned by year;
  [`npc_get_orgs()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_get_orgs.md)
  reads only the years requested.
  [`npc_standardize()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_standardize.md),
  [`npc_archive_dir()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_archive_dir.md)
  and
  [`npc_archive_info()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_archive_info.md)
  support it. Requires the arrow package.
- New population data on fixed 2020 boundaries: `pop_county` and
  `pop_cbsa` (2000-2025) and the county-to-CBSA crosswalk
  `cbsa_county_xwalk` (2023 delineation), read with
  [`npc_get_population()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_get_population.md).
- Metric tables now include a national row (`GEO = "ALL"`,
  `SUB = "ALL"`).
- [`check_year_completeness()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/check_year_completeness.md)
  judges years against the typical count of substantial years, so a tail
  of nearly empty early years no longer hides incomplete ones.
- Faster rollups with data.table grouping.
- The live NCCS source and the registered and grassroots universes are
  planned; requesting them gives an informative error.

### Metric fixes and tests (Phase 1)

- All concentration metrics share one set of helpers and one API:
  `geography`, `subsector`, `resource` and `year` arguments, with
  `SUB = "ALL"` and `GEO = "ALL"` rollups.
- [`get_scale_free_density()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_scale_free_density.md)
  now fits count models (negative binomial by default, or Poisson) on a
  full market frame that includes markets with no organizations, keeps
  national rows out of the models, and reports the density index as
  observed / expected - 1. The original log-linear model is available as
  `model = "loglinear"`.
- [`get_kwoka()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_kwoka.md)
  uses the standard Kwoka (1977) index, the sum of squared gaps between
  successive shares.
- Single-organization markets get `NHHI = 1`, consistent with HHI, CR*k*
  and Kwoka.
- New
  [`check_year_completeness()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/check_year_completeness.md);
  [`get_all_metrics()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_all_metrics.md)
  drops incomplete years by default.
- New dataset `npo_panel_sample` (Vermont 501(c)(3) filers, 2015-2019)
  for examples and tests.
- testthat suite with hand-computed values for every metric.

### Package cleanup (Phase 0)

- npcompete builds and passes `R CMD check`: scripts moved out of `R/`,
  one definition per function, complete `DESCRIPTION` and `NAMESPACE`.
- [`get_density()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_density.md),
  [`get_density_small()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_density_small.md),
  [`get_density_big()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_density_big.md),
  [`get_density_commercial()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_density_commercial.md)
  and
  [`get_fundraising_efficiency()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_fundraising_efficiency.md)
  use the old NCCS core-file layout and are deprecated.
- pkgdown site with the NODC pkgstyle template; MIT license.
