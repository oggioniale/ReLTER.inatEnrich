# Add nativeness information to iNaturalist occurrence records

**\[stable\]** Filters a tibble of iNaturalist occurrence records to
retain only research-grade, non-captive observations with a valid date,
then fetches establishment means information from iNaturalist for each
unique taxon via
[`get_nativeness_degree`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/get_nativeness_degree.md).
The results are joined back to the filtered occurrence tibble as a
nested `establishmentMeans` list-column.

If not already present, the function automatically assigns eLTER
Standard Observations to each record via `.assign_eLTER_SOs`, adding
three logical columns: `SOBIO_014` (Flying insects — Insecta),
`SOBIO_017` (Plants), and `SOBIO_018` (Acoustic recording — Aves, Anura,
Chiroptera, Orthoptera). Orthoptera contribute to SOBIO_014 and
SOBIO_018 simultaneously. If the columns are already present (e.g.
because a previous enrichment function was already run), the assignment
step is skipped.

## Usage

``` r
add_nativeness_to_occ(occ_eLTER, country)
```

## Arguments

- occ_eLTER:

  [`tibble`](https://tibble.tidyverse.org/reference/tibble.html). A
  tibble of iNaturalist occurrence records, typically obtained via
  `ReLTER::get_site_speciesOccurrences()`. Must contain at least the
  columns `quality_grade`, `observed_on`, `captive`, `taxon.id`, and
  `taxon.ancestor_ids`.

- country:

  `character`. The country name to filter establishment means by (e.g.,
  `"Italy"`). Must match the place name as recorded in iNaturalist.
  Cannot be `NULL`.

## Value

A [`tibble`](https://tibble.tidyverse.org/reference/tibble.html) of
filtered occurrence records (research-grade, non-captive, with valid
date) with the following additional columns:

- has_establishmentMeans:

  `logical`. Whether at least one valid (non-`NA`) establishment means
  value is available for the given `taxon.id`.

- establishmentMeans:

  list-column. Each element is a one-row tibble containing
  `iNat_nativeness`, `iNat_authority` (from iNaturalist), plus the EASIN
  fields `EASIN_url`, `EASIN_id`, `EASIN_LSID`,
  `EASIN_firstIntroductionsInEU_year`,
  `EASIN_firstIntroductions_Country`, `EASIN_status`, `EASIN_hasImpact`,
  `EASIN_IsEUConcern`. All fields are `NA` if not available.

- SOBIO_014:

  `logical`. Whether the observation contributes to Flying insects
  (SOBIO_014) eLTER Standard Observation. Assigned only if not already
  present.

- SOBIO_017:

  `logical`. Whether the observation contributes to Vegetation
  composition (SOBIO_017) eLTER Standard Observation. Assigned only if
  not already present.

- SOBIO_018:

  `logical`. Whether the observation contributes to Acoustic recording
  (SOBIO_018) eLTER Standard Observation. Assigned only if not already
  present.

## Note

The establishment means information is sourced from iNaturalist and may
refer to the IUCN Red List. It may not always be up to date.

eLTER Standard Observation assignments are based on taxonomic ancestry
(`taxon.ancestor_ids`) retrieved from the iNaturalist API.

Progress messages are printed to the console for each taxon processed,
including the iNaturalist taxon ID, nativeness status, and authority. A
summary of taxa with and without establishment means is printed at the
end.

The establishment means information is sourced from iNaturalist and may
refer to the IUCN Red List. It may not always be up to date.

## See also

[`get_nativeness_degree`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/get_nativeness_degree.md),
[`add_iucn_to_occ`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_iucn_to_occ.md),
[`add_eunis_legal_to_occ`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_eunis_legal_to_occ.md),
[`obs_SO_pie_chart`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/obs_SO_pie_chart.md)

## Author

Alessandro Oggioni, PhD <alessandro.oggioni@cnr.it>

Alice Lenzi, PhD <alice.lenzi@crea.gov.it>

Alessandro Campanaro, PhD <alessandro.campanaro@crea.gov.it>

## Examples

``` r
if (FALSE) { # \dontrun{
# Download taxa occurrences from iNaturalist using ReLTER's
# get_site_speciesOccurrences() function
# e.g. Montagna di Torricchio eLTER site
deimsid <- "https://deims.org/6b62feb2-61bf-47e1-b97f-0e909c408db8"

occ_eLTER <- ReLTER::get_site_speciesOccurrences(
  deimsid = deimsid,
  list_DS = "inat",
  show_map = FALSE,
  limit = 50
)

site_boundary <- ReLTER::get_site_info(deimsid = deimsid)

occ <- add_nativeness_to_occ(
  occ_eLTER = occ_eLTER$inat,
  country = site_boundary$country
)
} # }
```
