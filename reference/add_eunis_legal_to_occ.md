# Enrich iNaturalist occurrences with EUNIS legal framework information

**\[experimental\]** This function enriches iNaturalist occurrences with
legal protection information extracted from the EUNIS database. It uses
the scientific name retrieved from the iNaturalist `taxon.id` to query
EUNIS and extract legal directives related to the EU Habitats Directive
(92/43/EEC) and Birds Directive (2009/147/EC).

Observations are not filtered by geoprivacy or research grade. If a
taxon has no legal information in EUNIS, `NA` values are returned for
`directive` and `annex`.

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
add_eunis_legal_to_occ(occ_eLTER)
```

## Arguments

- occ_eLTER:

  A `tibble` containing iNaturalist occurrences. Must contain the
  columns `taxon.id` and `taxon.ancestor_ids`.

## Value

A `tibble` containing all original columns of `occ_eLTER` plus:

- directive:

  `character`. Legal directive text from EUNIS (92/43/EEC or
  2009/147/EC), or `NA` if not found.

- annex:

  `character`. Annex information from EUNIS table, or `NA` if not found.

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

eLTER Standard Observation assignments are based on taxonomic ancestry
(`taxon.ancestor_ids`) retrieved from the iNaturalist API.

## See also

[`get_eunis_legal_info`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/get_eunis_legal_info.md),
[`add_iucn_to_occ`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_iucn_to_occ.md),
[`add_nativeness_to_occ`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_nativeness_to_occ.md),
[`obs_SO_pie_chart`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/obs_SO_pie_chart.md)

## Author

Alessandro Oggioni, PhD (2023) <alessandro.oggioni@cnr.it>

## Examples

``` r
if (FALSE) { # \dontrun{
# Example: enrich iNaturalist occurrences with legal info
deimsid <- "https://deims.org/6b62feb2-61bf-47e1-b97f-0e909c408db8"
occ_iNat <- ReLTER::get_site_speciesOccurrences(
  deimsid = deimsid,
  list_DS = "inat",
  show_map = FALSE,
  limit = 5000
)

occ_legal <- add_eunis_legal_to_occ(
  occ_eLTER = occ_iNat$inat
)
} # }
```
