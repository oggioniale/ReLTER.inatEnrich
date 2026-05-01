# Get nativeness degree for a taxon from iNaturalist

**\[experimental\]** Queries the iNaturalist API to retrieve the
establishment means (nativeness status and authority) for a given taxon,
optionally filtered by country. Returns a one-row tibble with a nested
`establishmentMeans` list-column containing `nativeness` and
`authority`.

## Usage

``` r
get_nativeness_degree(taxon.id, country = NULL)
```

## Arguments

- taxon.id:

  `integer` or `character`. The iNaturalist taxon ID to query.

- country:

  `character`. The country name to filter results by (e.g., `"Italy"`).
  Must match the place name as recorded in iNaturalist. If `NULL`, a
  warning is issued and an empty result is returned to avoid ambiguous
  cross-country data.

## Value

A [`tibble`](https://tibble.tidyverse.org/reference/tibble.html) with
one row and one list-column:

- establishmentMeans:

  `list` of one-row tibbles, each containing:

  nativeness

  :   `character`. The establishment means value (e.g., `"native"`,
      `"introduced"`), or `NA` if not available.

  authority

  :   `character`. The authority or checklist title associated with the
      establishment means (e.g., `"Italy Check List"`), or `NA` if not
      available.

## Note

The establishment means information is sourced from iNaturalist and may
refer to the IUCN Red List. It may not always be up to date.

## See also

[`add_nativeness_to_occ`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_nativeness_to_occ.md)
for applying this function across a full occurrence tibble.

## Author

Alessandro Oggioni, PhD (2023) <alessandro.oggioni@cnr.it>

## Examples

``` r
if (FALSE) { # \dontrun{
# Get nativeness for a taxon in Italy
get_nativeness_degree(taxon.id = 48484, country = "Italy")
} # }
```
