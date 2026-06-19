# Get nativeness degree for a taxon from iNaturalist and EASIN

**\[stable\]** Queries the iNaturalist API to retrieve the establishment
means (nativeness status and authority) for a given taxon, optionally
filtered by country, as provided by iNaturalist checklists. Additionally
queries the EASIN (European Alien Species Information Network) database
to retrieve alien species information for the same taxon.

Data are sourced from:

1.  iNaturalist checklists —
    <https://forum.inaturalist.org/t/updating-iucn-red-list-conservation-statuses/25712>

2.  EASIN — European Alien Species Information Network —
    <https://easin.jrc.ec.europa.eu/easin>

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

  iNat_nativeness

  :   `character`. The establishment means value from iNaturalist (e.g.,
      `"native"`, `"introduced"`), or `NA` if not available.

  iNat_authority

  :   `character`. The checklist title associated with the establishment
      means in iNaturalist (e.g., `"Italy Check List"`), or `NA` if not
      available.

  iNat_checkList_uri

  :   `character`. The URL of the iNaturalist checklist associated with
      the establishment means, or `NA` if not available.

  EASIN_url

  :   `character`. URL of the EASIN species factsheet, or `NA` if the
      taxon is not in EASIN.

  EASIN_id

  :   `character`. EASIN species identifier, or `NA` if not available.

  EASIN_LSID

  :   `character`. Life Science Identifier for the species in EASIN
      (e.g., `"urn:lsid:easin.jrc.ec.europa.eu:species:XXXX"`), or `NA`
      if not available.

  EASIN_firstIntroductionsInEU_year

  :   `character`. Year of first introduction in the EU as recorded in
      EASIN, or `NA` if not available.

  EASIN_firstIntroductions_Country

  :   `character`. Country of first introduction in the EU as recorded
      in EASIN, or `NA` if not available.

  EASIN_status

  :   `character`. Alien species status as recorded in EASIN, or `NA` if
      not available.

  EASIN_hasImpact

  :   `character`. Whether the species has a documented impact as
      recorded in EASIN (`"True"` or `"False"`), or `NA` if not
      available.

  EASIN_IsEUConcern

  :   `character`. Whether the species is listed as a species of EU
      concern under Regulation (EU) No 1143/2014 (`"True"` or
      `"False"`), or `NA` if not available.

## Note

The establishment means information is sourced from iNaturalist and may
refer to the IUCN Red List. It may not always be up to date.

EASIN data are retrieved from the JRC API
(<https://easin.jrc.ec.europa.eu/apixg/catxg/term/>) and reflect the
information available in the EASIN catalogue. If the taxon is not
present in EASIN, all `EASIN_*` fields are `NA`.

**Disclaimer**: EASIN reports alien (i.e., non-native) status at the
country level and does not distinguish cases where a species is
non-native only in specific sub-national areas (e.g., islands) but
native in others within the same country. Therefore, EASIN information
may overgeneralize the nativeness status when applied to heterogeneous
territories such as Italy

## See also

[`add_nativeness_to_occ`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_nativeness_to_occ.md)
for applying this function across a full occurrence tibble.
`.assign_EASIN_info` for the underlying EASIN API call.

## Author

Alessandro Oggioni, PhD <alessandro.oggioni@cnr.it>

Alice Lenzi, PhD <alice.lenzi@crea.gov.it>

Alessandro Campanaro, PhD <alessandro.campanaro@crea.gov.it>

## Examples

``` r
if (FALSE) { # \dontrun{
# Get nativeness and EASIN info for a taxon in Italy
get_nativeness_degree(taxon.id = 48484, country = "Italy")

# Species of EU concern example
get_nativeness_degree(taxon.id = 61976, country = "Italy")
} # }
```
