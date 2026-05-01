# Add nativeness information to iNaturalist occurrence records

**\[experimental\]** Filters a tibble of iNaturalist occurrence records
to retain only research-grade, non-captive observations with a valid
date, then fetches establishment means information from iNaturalist for
each unique taxon via
[`get_nativeness_degree`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/get_nativeness_degree.md).
The results are joined back to the filtered occurrence tibble as a
nested `establishmentMeans` list-column.

## Usage

``` r
add_nativeness_to_occ(occ_eLTER, country)
```

## Arguments

- occ_eLTER:

  [`tibble`](https://tibble.tidyverse.org/reference/tibble.html). A
  tibble of iNaturalist occurrence records, typically obtained via
  `ReLTER::get_site_speciesOccurrences()`. Must contain at least the
  columns `quality_grade`, `observed_on`, `captive`, and `taxon.id`.

- country:

  `character`. The country name to filter establishment means by (e.g.,
  `"Italy"`). Must match the place name as recorded in iNaturalist.
  Cannot be `NULL`.

## Value

A [`tibble`](https://tibble.tidyverse.org/reference/tibble.html) of
filtered occurrence records (research-grade, non-captive, with valid
date) with an additional `establishmentMeans` list-column. Each element
of the list-column is a one-row tibble containing:

- nativeness:

  `character`. The establishment means value (e.g., `"native"`,
  `"introduced"`), or `NA` if not recorded in iNaturalist for the
  specified country.

- authority:

  `character`. The checklist or authority title associated with the
  establishment means, or `NA` if not available.

## Note

Progress messages are printed to the console for each taxon processed,
including the iNaturalist taxon ID, nativeness status, and authority. A
summary of taxa with and without establishment means is printed at the
end.

The establishment means information is sourced from iNaturalist and may
refer to the IUCN Red List. It may not always be up to date.

## See also

[`get_nativeness_degree`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/get_nativeness_degree.md)
for the underlying API call.

## Author

Alessandro Oggioni, PhD (2023) <alessandro.oggioni@cnr.it>

## Examples

``` r
if (FALSE) { # \dontrun{
deimsid <- "https://deims.org/6b62feb2-61bf-47e1-b97f-0e909c408db8"

occ_eLTER <- ReLTER::get_site_speciesOccurrences(
  deimsid = deimsid,
  list_DS = "inat",
  show_map = FALSE,
  limit = 5000
)

site_boundary <- ReLTER::get_site_info(deimsid = deimsid)
country <- site_boundary$country

occ <- add_nativeness_to_occ(
  occ_eLTER = occ_eLTER$inat,
  country = country
)
} # }
```
