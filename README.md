
<!-- README.md is generated from README.Rmd. Please edit that file. -->

# npcompete

<!-- badges: start -->

[![R-CMD-check](https://github.com/Nonprofit-Open-Data-Collective/npcompete/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Nonprofit-Open-Data-Collective/npcompete/actions/workflows/R-CMD-check.yaml)
[![License:
MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE.md)
<!-- badges: end -->

**npcompete** measures competition in local nonprofit markets. It takes
organization-level data, such as IRS Form 990 filings, and returns
concentration, inequality and density metrics for every market, defined
as a geography (metro area or county) crossed with a subsector and a
year.

The package answers questions like:

- Is a metro’s arts sector dominated by one or two large organizations,
  or spread across many?
- Does a county have more human-service nonprofits than you would expect
  for its population?
- How do these measures differ across subsectors and across places of
  different sizes?

## Installation

``` r
# install.packages( "remotes" )
remotes::install_github( "Nonprofit-Open-Data-Collective/npcompete" )
```

## Metrics

| Metric | Function | What it measures | Article |
|----|----|----|----|
| Herfindahl-Hirschman index (HHI) and normalized HHI | `get_hhi()`, `get_nhhi()` | Concentration: the sum of squared market shares | [HHI](https://nonprofit-open-data-collective.github.io/npcompete/articles/hhi.html) |
| Concentration ratios CR1 to CR8 | `get_cr()`, `get_cr_all()` | Share of the market held by the *k* largest organizations | [Concentration ratios](https://nonprofit-open-data-collective.github.io/npcompete/articles/concentration-ratios.html) |
| Kwoka dominance index | `get_kwoka()` | How far the leaders stand apart from the organizations behind them | [Kwoka](https://nonprofit-open-data-collective.github.io/npcompete/articles/kwoka.html) |
| Gini coefficient | `get_gini()` | Inequality of organization size | [Gini](https://nonprofit-open-data-collective.github.io/npcompete/articles/gini.html) |
| Scale-free density | `get_scale_free_density()` | More or fewer nonprofits (and revenue) than expected for the market’s population | [Density](https://nonprofit-open-data-collective.github.io/npcompete/articles/density.html) |
| All of the above | `get_all_metrics()` | Every metric in one table | [How the metrics relate](https://nonprofit-open-data-collective.github.io/npcompete/articles/metric-correlations.html) |

Concentration metrics can be measured on any resource column: total
revenue (`TOTREV`), total expenses (`TOTEXP`), net assets (`NETASS`) or
total assets (`TOTASS`).

## Quick start

`npo_panel_sample` holds every 501(c)(3) organization in Vermont that
filed a Form 990 or 990-EZ in tax years 2015 to 2019, one row per
organization-year. `get_all_metrics()` computes every metric for every
county, subsector and year:

``` r
library( npcompete )

m <- get_all_metrics( npo_panel_sample,
                      geography  = "COUNTY_FIPS",
                      population = "POP_COUNTY",
                      resource   = "TOTREV" )
```

The result has one row per market. Rows with `SUB = "ALL"` combine every
subsector in a county:

``` r
vars <- c( "GEO", "N", "POP", "HHI", "CR4", "GINI", "DENSITY_PCTL_N" )

subset( m, YEAR == 2019 & SUB == "ALL" & GEO != "ALL", vars ) |>
  head( 6 ) |>
  knitr::kable( digits = 2, row.names = FALSE )
```

| GEO   |   N |      POP |  HHI |  CR4 | GINI | DENSITY_PCTL_N |
|:------|----:|---------:|-----:|-----:|-----:|---------------:|
| 50001 | 123 |  37308.8 | 0.40 | 0.83 | 0.95 |           0.19 |
| 50003 | 124 |  37324.8 | 0.23 | 0.71 | 0.90 |           0.22 |
| 50005 |  82 |  30332.4 | 0.24 | 0.79 | 0.91 |           0.04 |
| 50007 | 414 | 167145.2 | 0.27 | 0.67 | 0.94 |          -0.27 |
| 50009 |   7 |   5958.6 | 0.23 | 0.85 | 0.48 |          -0.24 |
| 50011 |  53 |  49726.0 | 0.36 | 0.90 | 0.94 |          -0.63 |

Each metric also has its own function. The defaults expect the column
names of the NCCS panel (`CENSUS_CBSA_FIPS`, `NTEEV2`, `TOTREV`,
`TAXYEAR`), and any of them can be changed:

``` r
h <- get_hhi( npo_panel_sample, geography = "COUNTY_FIPS", resource = "NETASS" )

subset( h, YEAR == 2019 & GEO == "50007" ) |>
  knitr::kable( digits = 3, row.names = FALSE )
```

| GEO   | SUB | YEAR |   N |     NETASS |   HHI |
|:------|:----|-----:|----:|-----------:|------:|
| 50007 | ALL | 2019 | 414 | 2710000654 | 0.158 |
| 50007 | ART | 2019 |  39 |  138522398 | 0.185 |
| 50007 | EDU | 2019 |  53 |  154649672 | 0.222 |
| 50007 | ENV | 2019 |  28 |   53410815 | 0.209 |
| 50007 | HEL | 2019 |  56 |  297084111 | 0.086 |
| 50007 | HMS | 2019 | 159 |  411770664 | 0.134 |
| 50007 | HOS | 2019 |   2 | 1023427545 | 0.999 |
| 50007 | IFA | 2019 |   9 |    4994546 | 0.345 |
| 50007 | MMB | 2019 |   6 |  106226953 | 0.337 |
| 50007 | PSB | 2019 |  47 |   41603120 | 0.107 |
| 50007 | REL | 2019 |   6 |    2598427 | 0.433 |
| 50007 | UNI | 2019 |   4 |  474173650 | 0.318 |
| 50007 | UNU | 2019 |   5 |    1538753 | 0.403 |

## How markets are defined

- **Geography** is any column that identifies the market, usually a CBSA
  (metro or micro area) or a county.
- **Subsector** is taken from `NTEEV2`: its first three characters are
  the NTEE major group (`ART`, `EDU`, `HMS`, …), the NCCS version of the
  12 NTMAJ12 categories.
- **Rollups** are added automatically. `SUB = "ALL"` combines every
  subsector in a geography; `GEO = "ALL"` combines every geography in a
  subsector; `GEO = "ALL", SUB = "ALL"` is the national total.
- **Edge cases**: negative resource values are set to zero; a market
  with a single organization scores 1 on HHI, NHHI, CR*k* and Kwoka;
  markets whose resource total is zero get `NA`. Filter on `N` to study
  markets with several organizations.
- **Incomplete years**: IRS panels fill in over several years, so the
  latest tax years hold only part of the population. `get_all_metrics()`
  drops years flagged by `check_year_completeness()` unless told not to.

## Data included

| Dataset | Contents |
|----|----|
| `npo_panel_sample` | Vermont 501(c)(3) filers, 2015-2019, one row per organization-year (examples and tests) |
| `cbsa_metrics` | Every metric for every US metro and micro area in 2019, by subsector, on total revenue |
| `pop_county`, `pop_cbsa` | Annual county and CBSA population, 2000-2025 |
| `cbsa_county_xwalk` | County to CBSA crosswalk (2023 delineation) |

`cbsa_metrics` is a quick way to look at national patterns without
building anything:

``` r
# the most and least concentrated large metros, all subsectors combined
big <- subset( cbsa_metrics, SUB == "ALL" & POP > 2e6 )

# metro names from the population table
cbsa_names <- unique( pop_cbsa[ c( "CBSA_FIPS", "CBSA_NAME" ) ] )
big$METRO <- cbsa_names$CBSA_NAME[ match( big$GEO, cbsa_names$CBSA_FIPS ) ]

big <- big[ order( big$HHI ), c( "METRO", "POP", "N", "HHI", "CR4" ) ]

rbind( head( big, 3 ), tail( big, 3 ) ) |>
  knitr::kable( digits = 3, row.names = FALSE )
```

| METRO                                        |      POP |     N |   HHI |   CR4 |
|:---------------------------------------------|---------:|------:|------:|------:|
| New York-Newark-Jersey City, NY-NJ           | 19957716 | 28972 | 0.009 | 0.126 |
| Washington-Arlington-Alexandria, DC-VA-MD-WV |  6205437 | 12665 | 0.010 | 0.150 |
| Austin-Round Rock-San Marcos, TX             |  2226663 |  2718 | 0.012 | 0.146 |
| Columbus, OH                                 |  2115231 |  2953 | 0.145 | 0.600 |
| Pittsburgh, PA                               |  2456039 |  3708 | 0.183 | 0.617 |
| San Francisco-Oakland-Fremont, CA            |  4707646 |  8597 | 0.236 | 0.747 |

## Building metrics for many years

For national builds across years, npcompete reads from a local archive:
the NCCS SOI panel sample frame (about 2 GB as CSV) converted once to a
Parquet dataset partitioned by year. This needs the **arrow** and
**data.table** packages.

``` r
# where the archive lives (or set the NPCOMPETE_ARCHIVE environment variable)
options( npcompete.archive_dir = "D:/data/npcompete-archive" )

# one-time conversion of the SOI panel CSV to Parquet
npc_build_archive( "SOI-PANEL-SAMPLE-FRAME-COLUMNS-AUGMENTED-W-POP.CSV" )

# years held, row counts and which years are incomplete
npc_archive_info()

# every metric for every CBSA and county, 2012-2022, in one long table
m <- npc_build_metrics( 2012:2022,
                        geo       = c( "cbsa", "county" ),
                        resources = c( "TOTREV", "TOTEXP", "NETASS" ) )

# or the wide tables, written to disk with a manifest of the build
npc_build_metrics( 2012:2022, format = "wide", out_dir = "metrics" )
```

`npc_build_metrics()` attaches populations from `pop_county` and
`pop_cbsa` at the market level, so markets with no organizations in a
subsector are still part of the density models.

## Status and roadmap

The metric functions and the archive builder are stable and tested.
Planned work, described in [`dev/DEV-PLAN.md`](dev/DEV-PLAN.md):

- **Live NCCS source** (`source = "nccs"`): build metrics from NCCS
  downloads instead of a local archive.
- **Registered and grassroots universes** (`universe = "registered"`,
  `"grassroots"`): include organizations that file a 990-N or do not
  file, for count-based metrics.
- **Entry and exit dynamics**: entry, exit and churn rates by market.

Calling these options today gives an informative error.

### Legacy functions

`get_density()`, `get_density_small()`, `get_density_big()`,
`get_density_commercial()` and `get_fundraising_efficiency()` use the
old NCCS core-file layout (`MSA_NECH`, `NTMAJ12`, `FIPS`) and are
deprecated; use `get_scale_free_density()` for density. The
`nonprofit_sample` and `county_population` datasets and the precomputed
[`data-share/np-compete.csv`](data-share/) (1990-2019) belong to that
older version of the package.

## License

MIT. See [LICENSE.md](LICENSE.md).
