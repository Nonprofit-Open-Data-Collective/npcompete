# Building metrics from an archived panel

npcompete can build every metric for any set of years from a local
**archive**: a panel of Form 990 and 990-EZ filers stored as Parquet
files, one folder per tax year. The archive is built once from the NCCS
SOI panel sample frame (about 2 GB as CSV, 7 million
organization-years). After that, a build only reads the years it needs.

This article walks through the steps on a small stand-in for the full
panel: the Vermont 501(c)(3) filers in `npo_panel_sample` (2015-2019),
written out in the SOI panel layout. With the real file, the steps and
function calls are the same, but builds take minutes instead of seconds.

The archive needs the **arrow** and **data.table** packages.

``` r
library( npcompete )
```

## 1. Set where the archive lives

The archive should live outside the package and outside any synced
folder that might hold files online only. Point npcompete at it once per
session, or set the `NPCOMPETE_ARCHIVE` environment variable in
`.Renviron`.

``` r
options( npcompete.archive_dir = "D:/data/npcompete-archive" )
```

Here we use a temporary folder.

``` r
arch <- file.path( tempdir(), "npcompete-archive" )
options( npcompete.archive_dir = arch )
```

## 2. Build the archive

[`npc_build_archive()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_build_archive.md)
reads the SOI panel CSV, maps its columns to the package’s standard
layout with
[`npc_standardize()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_standardize.md),
and writes one Parquet file per tax year. With the full panel you would
run:

``` r
npc_build_archive( "SOI-PANEL-SAMPLE-FRAME-COLUMNS-AUGMENTED-W-POP.CSV" )
```

`npo_panel_sample` lacks a few columns of the full panel (source file,
e-file flag, fiscal year end, revocation dates), so for this example we
add them before writing the CSV.

``` r
soi <- npo_panel_sample
soi$SOURCE  <- ifelse( is.na( soi$TOTREV ), "", paste0( "SOI_", soi$TAXYEAR + 1, "_PC" ) )
soi$EFILE_X <- "EFILE"
soi$revocation_date <- ""
soi$exemption_reinstatement_date <- ""
soi$TOTLIA  <- NA
soi$REVINVE <- NA
soi$FISYEAR <- soi$TAXYEAR
soi$FISMO   <- 12

csv <- file.path( tempdir(), "soi-panel-vermont.csv" )
data.table::fwrite( soi, csv )

npc_build_archive( csv, overwrite = TRUE )
#> Reading soi-panel-vermont.csv ...
#> Standardizing 9,165 rows ...
#> Writing Parquet dataset to C:\Users\jlecy\AppData\Local\Temp\RtmpOcTw4M/npcompete-archive/filers ...
#> Done: 9,133 rows, years 2015-2019.
```

The archive records what it holds.
[`npc_archive_info()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_archive_info.md)
returns the build manifest and the rows per tax year, with a flag for
years that look incomplete. In the full panel, the most recent tax year
is always partial, because many organizations have not yet filed.
[`npc_build_metrics()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_build_metrics.md)
drops flagged years unless you ask it not to.

``` r
info <- npc_archive_info()
info$year_counts
#>   YEAR    N SHARE_OF_MEDIAN INCOMPLETE
#> 1 2015 1801           0.987      FALSE
#> 2 2016 1812           0.993      FALSE
#> 3 2017 1825           1.000      FALSE
#> 4 2018 1847           1.012      FALSE
#> 5 2019 1848           1.013      FALSE
```

Each organization appears once per tax year. Where the source panel
repeats an organization within a year (a short-period return after a
change of accounting period, or the same return in two extracts),
[`npc_standardize()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_standardize.md)
keeps one filing: the latest extract, then the latest period end.

## 3. Read organizations for some years

[`npc_get_orgs()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_get_orgs.md)
reads only the requested years. It keeps 501(c)(3) organizations by
default (`subsection = 3`). It also adds `CBSA_FIPS` from the
county-to-CBSA crosswalk (2023 delineation), so every year uses the same
metro boundaries.

``` r
orgs <- npc_get_orgs( 2018:2019 )
dim( orgs )
#> [1] 3695   26
head( orgs[ c( "EIN2", "YEAR", "COUNTY_FIPS", "CBSA_FIPS", "NTEEV2",
               "FILER_TYPE", "TOTREV" ) ] )
#>             EIN2 YEAR COUNTY_FIPS CBSA_FIPS     NTEEV2 FILER_TYPE  TOTREV
#> 1 EIN-01-0180687 2018       50027     30150 PSB-S30-RG        990   99714
#> 2 EIN-01-0180687 2019       50027     30150 PSB-S30-RG        990  920705
#> 3 EIN-01-0471151 2018       50023     12740 ENV-C40-RG        990 7980411
#> 4 EIN-01-0471151 2019       50023     12740 ENV-C40-RG        990 7861211
#> 5 EIN-01-0477533 2018       50023     12740 HEL-G32-RG        990  154658
#> 6 EIN-01-0477533 2019       50023     12740 HEL-G32-RG        990  160157
```

Rows marked `IMPUTED` fill a gap year between two filings: the
organization existed but did not file that year. They count toward
organization counts but carry no financial data. Drop them with
`include_imputed = FALSE`.

## 4. Build metrics

[`npc_build_metrics()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_build_metrics.md)
reads the years, attaches market populations from `pop_county` and
`pop_cbsa`, and computes each requested metric for each market (CBSA or
county), subsector (NTEE major group) and year. It also reports
`SUB = "ALL"` and `GEO = "ALL"` rollups.
[`npc_metrics()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_metrics.md)
lists every metric.

``` r
npc_metrics()[ c( "id", "family", "by_resource" ) ]
#>                        id      family by_resource
#> 1        DENSITY_PER_100K       count       FALSE
#> 2          DENSITY_PCTL_N       scale       FALSE
#> 3              MEDIAN_AGE         age       FALSE
#> 4        GRASSROOTS_SHARE  grassroots       FALSE
#> 5  SMALL_DENSITY_PER_100K        size        TRUE
#> 6  LARGE_DENSITY_PER_100K        size        TRUE
#> 7             SMALL_SHARE        size        TRUE
#> 8             LARGE_SHARE        size        TRUE
#> 9          DENSITY_PCTL_R       scale        TRUE
#> 10                    HHI         hhi        TRUE
#> 11                   NHHI         hhi        TRUE
#> 12                    CR1          cr        TRUE
#> 13                    CR2          cr        TRUE
#> 14                    CR3          cr        TRUE
#> 15                    CR4          cr        TRUE
#> 16                    CR5          cr        TRUE
#> 17                    CR6          cr        TRUE
#> 18                    CR7          cr        TRUE
#> 19                    CR8          cr        TRUE
#> 20                  KWOKA       kwoka        TRUE
#> 21                   GINI        gini        TRUE
#> 22       COMMERCIAL_SHARE  commercial       FALSE
#> 23  PROGRAM_REVENUE_SHARE  commercial       FALSE
#> 24       FUNDRAISER_SHARE fundraising       FALSE
#> 25      FUNDRAISING_RATIO fundraising       FALSE
#> 26             ENTRY_RATE    dynamics       FALSE
#> 27              EXIT_RATE    dynamics       FALSE
#> 28             NET_GROWTH    dynamics       FALSE
#> 29             CHURN_RATE    dynamics       FALSE
#> 30        GRADUATION_RATE    dynamics       FALSE
#> 31         DOWNGRADE_RATE    dynamics       FALSE
```

The default output is one long table, with one row per market,
subsector, year, resource and metric:

``` r
m <- npc_build_metrics( 2016:2019, geo = "county",
                        resources = c( "TOTREV", "NETASS" ),
                        metrics = c( "HHI", "CR4", "GINI", "DENSITY_PER_100K",
                                     "DENSITY_PCTL_N" ) )
#> Reading 4 year(s) from the archive ...
#> Building county metrics for TOTREV ...
#> Building county metrics for NETASS ...
vt <- substr( m$GEO, 1, 2 ) == "50"     # every US county is in the frame; show Vermont
head( m[ vt, ] )
#> # A tibble: 6 × 8
#>   GEO_LEVEL GEO   SUB    YEAR UNIVERSE RESOURCE METRIC                VALUE
#>   <chr>     <chr> <chr> <dbl> <chr>    <chr>    <chr>                 <dbl>
#> 1 county    50001 ALL    2016 filers   NA       N                      122 
#> 2 county    50001 ALL    2016 filers   NA       POP                  37146.
#> 3 county    50001 ALL    2016 filers   TOTREV   N_RES                  118 
#> 4 county    50001 ALL    2016 filers   TOTREV   RESOURCE_TOTAL   582431292 
#> 5 county    50001 ALL    2016 filers   NA       DENSITY_PER_100K       328.
#> 6 county    50001 ALL    2016 filers   NA       DENSITY_PCTL_N           2
```

Metrics that do not depend on a resource, such as density, appear once
with `RESOURCE = NA`.

Here is concentration of revenue in Chittenden County (Burlington)
across all subsectors:

``` r
subset( m, GEO == "50007" & SUB == "ALL" & METRIC %in% c( "HHI", "CR4" ) &
           RESOURCE == "TOTREV" )
#> # A tibble: 8 × 8
#>   GEO_LEVEL GEO   SUB    YEAR UNIVERSE RESOURCE METRIC VALUE
#>   <chr>     <chr> <chr> <dbl> <chr>    <chr>    <chr>  <dbl>
#> 1 county    50007 ALL    2016 filers   TOTREV   HHI    0.269
#> 2 county    50007 ALL    2016 filers   TOTREV   CR4    0.661
#> 3 county    50007 ALL    2017 filers   TOTREV   HHI    0.267
#> 4 county    50007 ALL    2017 filers   TOTREV   CR4    0.660
#> 5 county    50007 ALL    2018 filers   TOTREV   HHI    0.277
#> 6 county    50007 ALL    2018 filers   TOTREV   CR4    0.679
#> 7 county    50007 ALL    2019 filers   TOTREV   HHI    0.273
#> 8 county    50007 ALL    2019 filers   TOTREV   CR4    0.677
```

### Wide tables

`format = "wide"` returns one table per geography and resource, with
metrics as columns. This is the layout of
[`get_all_metrics()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/get_all_metrics.md)
and of the files the dashboard reads.

``` r
w <- npc_build_metrics( 2019, geo = "cbsa", resources = "TOTREV",
                        metrics = c( "HHI", "NHHI", "KWOKA", "DENSITY_PCTL_N" ),
                        format = "wide" )
#> Reading 1 year(s) from the archive ...
#> Building cbsa metrics for TOTREV ...
names( w )
#> [1] "cbsa_TOTREV"
t <- w$cbsa_TOTREV
subset( t, SUB == "ALL" & N > 0 )
#> # A tibble: 7 × 11
#>   GEO   SUB    YEAR     N        POP N_RES   TOTREV DENSITY_PCTL_N    HHI   NHHI
#>   <chr> <chr> <dbl> <dbl>      <dbl> <dbl>    <dbl>          <dbl>  <dbl>  <dbl>
#> 1 12740 ALL    2019   258     59780.   251   8.93e8              2 0.128  0.124 
#> 2 13540 ALL    2019   129     37325.   124   4.07e8              2 0.225  0.219 
#> 3 14710 ALL    2019   191     45766.   181   4.94e8              2 0.122  0.117 
#> 4 15540 ALL    2019   491    224132.   480   3.30e9              2 0.239  0.238 
#> 5 30150 ALL    2019   291    220936.   281   6.67e8              2 0.0593 0.0559
#> 6 40860 ALL    2019   125     60679    117   4.66e8              2 0.423  0.418 
#> 7 ALL   ALL    2019  1848 332508258.  1783   7.45e9             NA 0.0556 0.0551
#> # ℹ 1 more variable: KWOKA <dbl>
```

In this sample `DENSITY_PCTL_N` sits at its cap of 2 for every Vermont
metro. The density model compares each market with every other market in
the country, and in this sample only Vermont has any organizations. With
the full panel, values spread around 0.

### Pooled windows for small markets

Small markets have few organizations, so their metrics jump from year to
year. `window = 3` pools three years for each reported year:

- each organization’s financial values are averaged over the years it
  appears;
- its location and subsector come from its latest year;
- population is the three-year average.

Reported years without a full window of data are dropped.

``` r
w3 <- npc_build_metrics( 2017:2019, geo = "county", window = 3,
                         metrics = c( "DENSITY_PER_100K", "MEDIAN_AGE" ),
                         format = "wide" )
#> Reading 5 year(s) from the archive ...
#> Building county metrics ...
subset( w3$county, SUB == "ALL" & substr( GEO, 1, 2 ) == "50" & YEAR == 2019 )
#> # A tibble: 14 × 8
#>    GEO   SUB    YEAR     N     POP N_AGE DENSITY_PER_100K MEDIAN_AGE
#>    <chr> <chr> <dbl> <dbl>   <dbl> <int>            <dbl>      <dbl>
#>  1 50001 ALL    2019   131  37255.   129             352.       23  
#>  2 50003 ALL    2019   134  37303.   131             359.       24  
#>  3 50005 ALL    2019    89  30432.    86             292.       22  
#>  4 50007 ALL    2019   452 165967.   446             272.       17  
#>  5 50009 ALL    2019    10   5997.    10             167.       17.5
#>  6 50011 ALL    2019    58  49506     57             117.       20  
#>  7 50013 ALL    2019    17   7228.    16             235.       23.5
#>  8 50015 ALL    2019    91  25651     89             355.       19  
#>  9 50017 ALL    2019    82  29209.    81             281.       23  
#> 10 50019 ALL    2019    60  27361.    59             219.       24  
#> 11 50021 ALL    2019   134  60786    129             220.       21  
#> 12 50023 ALL    2019   277  59752.   274             464.       21  
#> 13 50025 ALL    2019   196  45627.   192             430.       21  
#> 14 50027 ALL    2019   223  57536.   221             388.       21
```

### Writing results with a manifest

With `out_dir`, the results are written as CSV along with
`manifest.csv`. The manifest records the archive used, the years,
metrics, options and package version, so the build can be reproduced or
compared with a later one.

``` r
out <- file.path( tempdir(), "metrics" )
res <- npc_build_metrics( 2018:2019, geo = "cbsa", resources = "TOTREV",
                          metrics = c( "HHI", "GINI" ), format = "wide",
                          out_dir = out )
#> Reading 2 year(s) from the archive ...
#> Building cbsa metrics for TOTREV ...
#> Wrote results to C:\Users\jlecy\AppData\Local\Temp\RtmpOcTw4M/metrics
list.files( out )
#> [1] "manifest.csv"                        
#> [2] "NPCOMPETE-METRICS-BY-CBSA-TOTREV.CSV"
head( attr( res, "manifest" ), 12 )
#>                 key
#> 1  data_source_file
#> 2        data_built
#> 3          universe
#> 4            source
#> 5             years
#> 6            window
#> 7               geo
#> 8         resources
#> 9           metrics
#> 10       subsection
#> 11  include_imputed
#> 12 exclude_churches
#>                                                                 value
#> 1  C:/Users/jlecy/AppData/Local/Temp/RtmpOcTw4M/soi-panel-vermont.csv
#> 2                                                 2026-11-25 00:41:16
#> 3                                                              filers
#> 4                                                             archive
#> 5                                                           2018,2019
#> 6                                                                   1
#> 7                                                                cbsa
#> 8                                                              TOTREV
#> 9                                                            HHI,GINI
#> 10                                                                  3
#> 11                                                               TRUE
#> 12                                                              FALSE
```

## Choices that change the results

- **`cbsa`**: `"crosswalk"` (default) assigns CBSAs from each county
  under the 2023 delineation, the same boundaries as the population
  data. `"geocode"` uses the CBSA assigned when the address was
  geocoded, which reproduces tables built before this option existed.
- **`subsection`**: 501(c)(3) only by default; `NULL` keeps every
  subsection.
- **`density_model`**: the count model behind `DENSITY_PCTL_N`, negative
  binomial by default (see
  [`vignette("density")`](https://nonprofit-open-data-collective.github.io/npcompete/articles/density.md)).
- **`drop_incomplete_years`**: keep partial years only when you know why
  you want them.

To build the same metrics without the SOI panel, from public NCCS files,
see
[`vignette("build-from-nccs")`](https://nonprofit-open-data-collective.github.io/npcompete/articles/build-from-nccs.md).
