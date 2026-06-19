# Enrich iNaturalist specific project observations with IUCN conservation status

**\[experimental\]** This function enriches all the iNaturalist project
observations with the IUCN Red List conservation status thanks to the
[`get_conservation_status()`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/get_conservation_status.md)
function. The observations are filtered to include only 'Research grade'
data also that meet the following a valid date, an 'open' geographic
location, the presence of a photo or sound, and the exclusion of captive
or cultivated organisms.

## Usage

``` r
add_iucn_to_obs(project_name)
```

## Arguments

- project_name:

  A `string` value representing the name of the iNaturalist project.

## Value

A `data.frame` object representing the iNaturalist project observations,
as well as the conservation status of the taxa. The structure of the
`data.frame` follows the original structure as obtained by the
[`rinat::get_inat_obs_project()`](https://docs.ropensci.org/rinat/reference/get_inat_obs_project.html)
function where the type arguments is set to "observations". The content
of the `data.frame` returned is enriched with the following columns:

- `info_title_proj`: A `character` value representing the title of the
  project, as obtained by the
  [`rinat::get_inat_obs_project()`](https://docs.ropensci.org/rinat/reference/get_inat_obs_project.html)
  function where the type arguments is set to "info";

- `info_slug_proj`: A `character` value representing the slug of the
  project;

- `info_taxa_num_proj`: A `numeric` value representing the number of
  taxa in the project;

- `info_place_uuid_proj`: A `character` value representing the place
  uuid of the project within iNaturalist system;

- `status`: A `character` value representing the conservation status of
  the taxa by parsing the Taxa iNaturalist API JSON response, based on
  the `taxon.id` value and only if the authority is the 'IUCN Red List'.
  In the case the JSON parsing fails, it will returns an Error message
  and fill this column with NA. The function also prints some console
  messages to inform the user about the status results for each
  `taxon.id`, as well as the number of taxa that encountered a JSON
  parsing error.

## Author

Alessandro Oggioni, PhD <alessandro.oggioni@cnr.it>

Alice Lenzi, PhD <alice.lenzi@crea.gov.it>

Alessandro Campanaro, PhD <alessandro.campanaro@crea.gov.it>

## Examples

``` r
if (FALSE) { # \dontrun{
## Not run:
add_iucn_to_obs(project_name = "LTER site Montagna di Torricchio")
} # }
## End (Not run)
```
