# Download a file from the NCCS data bucket

Downloads one file to `dir/raw/`, retrying on failure, and records the
URL, size, MD5 hash and time in `dir/downloads.csv`. A file already in
the cache is reused unless `refresh = TRUE`.

## Usage

``` r
npc_nccs_download(
  key,
  dir = npc_cache_dir(),
  refresh = FALSE,
  retries = 3,
  quiet = FALSE
)
```

## Arguments

- key:

  Path of the file in the bucket, for example
  `"processed_merged/core/2019/990combined/core_2019_990combined.parquet"`.

- dir:

  Cache directory; see
  [`npc_cache_dir()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_cache_dir.md).

- refresh:

  Download again even if the file is cached.

- retries:

  Number of attempts before giving up.

- quiet:

  Suppress progress messages.

## Value

The local path of the file.

## Examples

``` r
if (FALSE) { # \dontrun{
npc_nccs_download( "harmonized/core/CORE-HRMN_dd.csv" )
} # }
```
