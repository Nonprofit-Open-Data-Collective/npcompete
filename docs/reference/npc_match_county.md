# Match county names to county FIPS codes

Finds the five-digit FIPS code for county names such as those returned
by a geocoder ("Cook County", "Norfolk city", "Lafourche Parish",
"Municipality of Anchorage", "City of Baltimore"). Names are matched
within the state against
[pop_county](https://nonprofit-open-data-collective.github.io/npcompete/reference/pop_county.md),
ignoring case, accents, punctuation and the type of county. Where a
state has both a county and an independent city of the same name
(Baltimore, St. Louis, Richmond VA, ...), names marked as a city match
the independent city.

## Usage

``` r
npc_match_county(county, state)
```

## Arguments

- county:

  County names.

- state:

  Two-letter state abbreviations, the same length as `county`.

## Value

A character vector of FIPS codes, `NA` where no county matched.

## Examples

``` r
npc_match_county( c( "Cook County", "City of Baltimore", "Baltimore County",
                     "Doña Ana County" ),
                  c( "IL", "MD", "MD", "NM" ) )
#> [1] "17031" "24510" "24005" "35013"
```
