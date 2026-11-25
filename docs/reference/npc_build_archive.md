# Build the local Parquet archive from the SOI panel CSV

Reads the SOI panel sample frame CSV once, converts it to the canonical
schema with
[`npc_standardize()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_standardize.md),
and writes it to `dir/filers/` as a Parquet dataset partitioned by
`YEAR`. Later reads with
[`npc_get_orgs()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_get_orgs.md)
only touch the years requested. A `manifest.csv` and `year-counts.csv`
record what was built.

## Usage

``` r
npc_build_archive(csv, dir = npc_archive_dir(), overwrite = FALSE)
```

## Arguments

- csv:

  Path to `SOI-PANEL-SAMPLE-FRAME-COLUMNS-AUGMENTED*.CSV`.

- dir:

  Archive directory; see
  [`npc_archive_dir()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_archive_dir.md).

- overwrite:

  Replace an existing `filers` dataset.

## Value

The archive directory, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
npc_build_archive( "SOI-PANEL-SAMPLE-FRAME-COLUMNS-AUGMENTED-W-POP.CSV",
                   dir = "D:/data/npcompete-archive" )
} # }
```
