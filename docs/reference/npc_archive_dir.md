# Location of the local data archive

The archive directory is taken from `options(npcompete.archive_dir = )`,
then the `NPCOMPETE_ARCHIVE` environment variable.

## Usage

``` r
npc_archive_dir(dir = NULL)
```

## Arguments

- dir:

  Optional directory; returned as is when supplied.

## Value

A directory path.

## Examples

``` r
if (FALSE) { # \dontrun{
options( npcompete.archive_dir = "D:/data/npcompete-archive" )
npc_archive_dir()
} # }
```
