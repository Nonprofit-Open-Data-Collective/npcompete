# Describe the local archive or a live NCCS build

Works for both the SOI panel archive
([`npc_build_archive()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_build_archive.md))
and a live build
([`npc_build_nccs()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_build_nccs.md)),
which share a layout.

## Usage

``` r
npc_archive_info(dir = npc_archive_dir())
```

## Arguments

- dir:

  Archive or cache directory.

## Value

A list with `manifest` (key/value data frame) and `year_counts` (rows
per year with completeness flags from
[`check_year_completeness()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/check_year_completeness.md)).
