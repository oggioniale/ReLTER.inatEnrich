# Function to obtain IUCN conservation status for a single taxon.id from iNaturalist API

**\[experimental\]** This function generates a request to the
iNaturalist API of a given `taxon.id` to obtain, if updated, the IUCN
conservation status, where the authority is the "IUCN Red List".

Registered user can update iNaturalist IUCN conservation status. For
more information about the procedure for updating the IUCN conservation
status in iNaturalist community, please refer to the following link:
https://forum.inaturalist.org/t/updating-iucn-red-list-conservation-statuses/25712

For more information about iNaturalist API of Taxa, please refer to the
following link: https://api.inaturalist.org/v1/docs/#!/Taxa/get_taxa_id
using as "id" value 517449. The JSON key that contains info about IUCN
conservation staus is 'conservation_statuses\[\]'.

## Usage

``` r
get_conservation_status(taxon.id)
```

## Arguments

- taxon.id:

  A `number` value representing the taxon.id of the taxa in iNaturalist
  database.

## Value

A `tibble` containing the conservation status information of the taxa
retrieved from iNaturalist. The tibble includes the columns `status`,
`authority`, `name`, and `url`. If the parsing fails or no IUCN Red List
conservation status is available, the function returns a tibble with
`NA` values in all fields.

## Author

Alessandro Oggioni, phD (2023) <alessandro.oggioni@cnr.it>

## Examples

``` r
if (FALSE) { # \dontrun{
## Not run:
get_conservation_status(
  taxon.id = 517449
)

get_conservation_status(
  taxon.id = 632126
)

get_conservation_status(
  taxon.id = 472766
)

} # }
## End (Not run)
```
