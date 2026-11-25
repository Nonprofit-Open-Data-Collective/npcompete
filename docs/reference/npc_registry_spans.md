# Registration spans from the BMF and the revocation list

One row per EIN on the unified BMF with the years it was registered:
`START` (ruling year or first BMF vintage, whichever is earlier), `END`
(year of the last BMF vintage, or `NA` if still listed), and the latest
automatic revocation and reinstatement. Used by
[`npc_build_registered()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_build_registered.md);
see there for the rules.

## Usage

``` r
npc_registry_spans(dir = npc_cache_dir(), refresh = FALSE)
```

## Arguments

- dir:

  Cache directory; see
  [`npc_cache_dir()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_cache_dir.md).
  Downloads and the registered dataset go here.

- refresh:

  Download the BMF, revocation and e-postcard files again.

## Value

A data frame with `EIN2`, `COUNTY_FIPS`, `NTEEV2`, `SUBSECTION`,
`RULING_YEAR`, `FILING_REQ_CODE`, `CHURCH`, `FOUNDATION`, `START`,
`END`, `REVOCATION_YEAR` and `REINSTATEMENT_YEAR`.
