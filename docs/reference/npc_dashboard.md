# Explore metrics in an interactive dashboard

Opens a Shiny app for browsing the output of
[`npc_build_metrics()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_build_metrics.md):
pick a universe, geography, metric, resource, subsector, year and
market, and see the market against the distribution of all markets, over
time, against a second metric, and across subsectors. The table tab
downloads the selection as CSV.

## Usage

``` r
npc_dashboard(data = NULL, launch = interactive(), ...)
```

## Arguments

- data:

  Metrics to show. Any of:

  - the long output of
    [`npc_build_metrics()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_build_metrics.md)
    (or several, combined with
    [`rbind()`](https://rdrr.io/r/base/cbind.html), for example one per
    universe), or a list of them;

  - its wide output (`format = "wide"`), a named list of tables;

  - a folder written with `out_dir`, or a path to a CSV written there;

  - `NULL` (default), the bundled
    [cbsa_metrics](https://nonprofit-open-data-collective.github.io/npcompete/reference/cbsa_metrics.md)
    for 2019.

- launch:

  Start the app. With `FALSE`, return the app object, for example to
  deploy it with
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html) or on a
  Shiny server.

- ...:

  Passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

The Shiny app object, invisibly when launched.

## Examples

``` r
if (FALSE) { # \dontrun{
npc_dashboard()      # bundled 2019 CBSA metrics

f <- npc_build_metrics( 2015:2021, geo = c( "cbsa", "county" ), source = "nccs" )
r <- npc_build_metrics( 2015:2021, geo = c( "cbsa", "county" ), source = "nccs",
                        universe = "registered" )
npc_dashboard( rbind( f, r ) )
} # }
```
