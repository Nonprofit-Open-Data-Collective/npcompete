# Location of the NCCS download cache

Live builds
([`npc_build_nccs()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_build_nccs.md),
or `source = "nccs"` in
[`npc_get_orgs()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_get_orgs.md)
and
[`npc_build_metrics()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_build_metrics.md))
keep downloaded NCCS files and the harmonized organization table here.
The location is taken from `options(npcompete.cache_dir = )`, then the
`NPCOMPETE_CACHE` environment variable, then
`tools::R_user_dir("npcompete", "cache")`.

## Usage

``` r
npc_cache_dir(dir = NULL)
```

## Arguments

- dir:

  Optional directory; returned as is when supplied.

## Value

A directory path.

## Examples

``` r
npc_cache_dir()
#> [1] "C:\\Users\\jlecy\\AppData\\Local/R/cache/R/npcompete"
```
