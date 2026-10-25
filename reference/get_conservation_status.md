# Function to obtain IUCN conservation status for a single taxon.id from iNaturalist API This function generates a request to the iNaturalist API of a given `taxon.id` to obtain, if available, the IUCN conservation status. By default only the global assessment (authority: `"IUCN Red List"`) is returned. If a `country` is specified, the country-level assessment is also included when available.

Registered user can update iNaturalist IUCN conservation status. For
more information about the procedure for updating the IUCN conservation
status in iNaturalist community, please refer to the following link:
https://forum.inaturalist.org/t/updating-iucn-red-list-conservation-statuses/25712

## Usage

``` r
get_conservation_status(taxon.id, country = NULL)
```

## Arguments

- taxon.id:

  A `number` value representing the taxon.id of the taxa in iNaturalist
  database.

- country:

  An optional character string specifying the country name (e.g.
  `"Italy"`) to include country-level IUCN assessments alongside the
  global one. When provided, rows from the iNaturalist conservation
  statuses API whose place name matches the given country are included
  even if their authority is not `"IUCN Red List"` (e.g.
  `"IUCN Liste Rosse Italiane"`). If `NULL` (default), only the global
  IUCN Red List assessment is returned.

## Value

A `tibble` containing the conservation status information of the taxa
retrieved from iNaturalist. The tibble includes the columns `status`,
`authority`, `scope_of_assesment`, and `url`. When `country` is
provided, the tibble may contain both a global row (authority
`"IUCN Red List"`) and a country-level row. If the parsing fails or no
matching conservation status is available, the function returns a tibble
with `NA` values in all fields.

## Details

For more information about iNaturalist API of Taxa, please refer to the
following link: https://api.inaturalist.org/v1/docs/#!/Taxa/get_taxa_id
using as "id" value 517449. The JSON key that contains info about IUCN
conservation staus is 'conservation_statuses\[\]'.

## Author

Alessandro Oggioni, PhD <alessandro.oggioni@cnr.it>

Alice Lenzi, PhD <alice.lenzi@crea.gov.it>

Alessandro Campanaro, PhD <alessandro.campanaro@crea.gov.it>

## Examples

``` r
if (FALSE) { # \dontrun{
## Not run:
# with 1 IUCN red list conservation status declared
# Sclerophrys pantherina
get_conservation_status(
  taxon.id = 517449
)

# without IUCN red list conservation status declared
# Protoparmeliopsis muralis
get_conservation_status(
  taxon.id = 632126
)

# with 3 IUCN red list conservation status declared
# Falco tinnunculus
get_conservation_status(
  taxon.id = 472766
)

} # }
## End (Not run)
```
