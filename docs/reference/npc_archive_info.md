# Describe the local archive

Describe the local archive

## Usage

``` r
npc_archive_info(dir = npc_archive_dir())
```

## Arguments

- dir:

  Archive directory; see
  [`npc_archive_dir()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_archive_dir.md).

## Value

A list with `manifest` (key/value data frame) and `year_counts` (rows
per year with completeness flags from
[`check_year_completeness()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/check_year_completeness.md)).
