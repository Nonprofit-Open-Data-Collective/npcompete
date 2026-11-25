# Convert an organization panel to the canonical npcompete schema

Maps the columns of a source panel onto the organization-year schema
that the archive and
[`npc_build_metrics()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_build_metrics.md)
use, so that every data source ends up in the same layout.

## Usage

``` r
npc_standardize(df, source = "soi_panel")
```

## Arguments

- df:

  A data frame from the source panel.

- source:

  Which source layout `df` follows. Currently only `"soi_panel"`, the
  NCCS SOI panel sample frame of Form 990 and 990-EZ filers.

## Value

A data frame with one row per organization-year and columns:

- EIN2:

  Employer identification number (`EIN-XX-XXXXXXX`)

- YEAR:

  Tax year

- COUNTY_FIPS:

  Five-digit county FIPS code

- CBSA_FIPS_GEOCODE:

  CBSA assigned when the address was geocoded;
  [`npc_get_orgs()`](https://nonprofit-open-data-collective.github.io/npcompete/reference/npc_get_orgs.md)
  adds `CBSA_FIPS` from the county crosswalk

- NTEEV2:

  NTEE code in NCCS version 2 format

- SUBSECTION:

  501(c) subsection

- FILER_TYPE:

  `"990"`, `"990EZ"`, or `NA` for imputed gap years

- EFILE:

  `"EFILE"`, `"PAPER"` or `NA`

- RULING_YEAR:

  Year the IRS granted exemption

- REVOKED:

  `TRUE` in the year the exemption was revoked

- REVOCATION_YEAR, REINSTATEMENT_YEAR:

  Years of automatic revocation and reinstatement, if any

- TOTREV, TOTEXP, NETASS, TOTASS, TOTLIA:

  Revenue, expenses, net assets, assets and liabilities

- REVPROG, REVCONT, REVINVE, FNDFEE:

  Program revenue, contributions, investment income and professional
  fundraising fees

- FISYEAR, FISMO:

  Fiscal year and month the tax period ended

- IMPUTED:

  `TRUE` for rows that fill a gap between filings; they have no
  financial data

- SOURCE:

  The source file the row came from

## Examples

``` r
raw <- data.frame(
  EIN2 = "EIN-01-0000001", TAXYEAR = 2019, COUNTY_FIPS = 1001,
  CENSUS_CBSA_FIPS = 33860, NTEEV2 = "ART-A20-RG", V501C = 3,
  SOURCE = "SOI_2020_PC", EFILE_X = "EFILE", ORG_RULING_YEAR = 1990,
  REVOKED_YEAR_X = 0, revocation_date = "", exemption_reinstatement_date = "",
  TOTREV = 1000, TOTEXP = 900, NETASS = 50, TOTASS = 80, TOTLIA = 30,
  REVPROG = 100, REVCONT = 800, REVINVE = 10, FNDFEE = NA, FISYEAR = 2019,
  FISMO = 12, MISSING_YEAR = 0 )
npc_standardize( raw )
#>             EIN2 YEAR COUNTY_FIPS CBSA_FIPS_GEOCODE     NTEEV2 SUBSECTION
#> 1 EIN-01-0000001 2019       01001             33860 ART-A20-RG          3
#>   FILER_TYPE EFILE RULING_YEAR REVOKED REVOCATION_YEAR REINSTATEMENT_YEAR
#> 1        990 EFILE        1990   FALSE              NA                 NA
#>   TOTREV TOTEXP NETASS TOTASS TOTLIA REVPROG REVCONT REVINVE FNDFEE FISYEAR
#> 1   1000    900     50     80     30     100     800      10     NA    2019
#>   FISMO IMPUTED      SOURCE
#> 1    12   FALSE SOI_2020_PC
```
