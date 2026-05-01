# Enrich eLTER site iNaturalist occurrences with IUCN conservation status

**\[experimental\]** This function enriches all the eLTER site
iNaturalist occurrences acquired by the
`ReLTER::get_site_speciesOccurrences()` function with the IUCN Red List
conservation status as recorded in iNaturalist.

Observations are filtered to include only "Research Grade" data that
meet the following criteria: have a valid date and exclude captive or
cultivated organisms.

Observations are not filtered with respect to geoprivacy. In
iNaturalist, each observation can be assigned one of three geoprivacy
levels: open, obscured, or private (for more information, see:
https://www.inaturalist.org/pages/geoprivacy). Given the need to gather
information on species with critical conservation status, even if the
geographic information is not precise, it is still important to record
the presence of a species within a given area.

The function queries the iNaturalist Taxa API once per unique `taxon.id`
and stores the returned IUCN assessments (if available) in a nested
list-column.

## Usage

``` r
add_iucn_to_occ(occ_eLTER)
```

## Arguments

- occ_eLTER:

  A `data.frame` or `sf` object representing the occurrences of an eLTER
  site acquired by the `ReLTER::get_site_speciesOccurrences()` function
  (typically `occ_eLTER$inat`).

## Value

A `data.frame` or `sf` object with the same structure as the input,
enriched with the following additional columns:

- status_IUCN:

  A list-column (`list` of `tibble`s) containing the IUCN Red List
  assessments retrieved from iNaturalist for each `taxon.id`. Each
  tibble has the following columns: `status`, `authority`, `name`,
  `url`. If no IUCN information is available, a tibble with `NA` values
  is stored.

- has_IUCN:

  A `logical` value indicating whether at least one valid (non-`NA`)
  IUCN status is available for the given `taxon.id`.

The function also prints informative console messages reporting:

- the progress of API queries for each `taxon.id`

- whether IUCN data are available or missing

- a summary of taxa with and without IUCN status

## Details

The IUCN conservation status is retrieved from the iNaturalist Taxa API
and reflects the information available within iNaturalist. This
information may not be up to date with respect to the official IUCN Red
List.

## Author

Alessandro Oggioni, PhD (2023) <alessandro.oggioni@cnr.it>

## Examples

``` r
if (FALSE) { # \dontrun{
## Not run:
# Download taxa occurrences from iNaturalist using ReLTER's
# get_site_speciesOccurrences() function
# e.g. Montagna di Torricchio eLTER site
deimsid <- "https://deims.org/6b62feb2-61bf-47e1-b97f-0e909c408db8"

occ_eLTER <- ReLTER::get_site_speciesOccurrences(
  deimsid = deimsid,
  list_DS = "inat",
  show_map = FALSE,
  limit = 5000
)

occ <- add_iucn_to_occ(
  occ_eLTER = occ_eLTER$inat
)
} # }
## End (Not run)
```
