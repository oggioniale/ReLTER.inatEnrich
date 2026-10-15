# Get EU directive legal information for a species using the BISE API

**\[stable\]**

Takes an iNaturalist `taxon.id`, retrieves the species scientific name
from the iNaturalist Taxa API, then queries the
[BISE](https://biodiversity.europa.eu) (Biodiversity Information System
for Europe) REST API in two steps:

1.  Species search by scientific name to obtain the internal `id_eunis`
    identifier.

2.  Legal status lookup by `id_eunis` to obtain EU directive and annex
    information from the Protection table.

Only rows where `legal_instrument` is `"EU Habitats Directive"` or
`"EU Birds Directive"` are retained.

## Usage

``` r
get_eunis_legal_info(taxon.id)
```

## Arguments

- taxon.id:

  Integer. The taxon ID of the species in iNaturalist.

## Value

A tibble with one row per EU directive entry for the species, or a
single-row tibble with `NA` values if the species is not found. Columns:

- taxon.id:

  iNaturalist taxon ID.

- scientific_name:

  Scientific name from iNaturalist.

- directive:

  `"Habitats Directive (92/43/EEC)"`, `"Birds Directive (2009/147/EC)"`,
  or `NA`.

- annex:

  Short annex code parsed from the BISE annex description, e.g. `"II"`,
  `"IV"`, `"I"`, or `NA`.

## Author

Alessandro Oggioni, PhD <alessandro.oggioni@cnr.it>

## Examples

``` r
if (FALSE) { # \dontrun{
# Canis lupus -- Habitats Directive, Annex II and IV
get_eunis_legal_info(taxon.id = 42048)

# Falco tinnunculus -- Birds Directive
get_eunis_legal_info(taxon.id = 5221)
} # }
```
