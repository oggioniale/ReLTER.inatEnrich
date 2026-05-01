# Enrich iNaturalist occurrences with EUNIS legal framework information

**\[experimental\]** This function enriches iNaturalist occurrences with
legal protection information extracted from the EUNIS database. It uses
the scientific name retrieved from the iNaturalist `taxon.id` to query
EUNIS and extract legal directives related to the EU Habitats Directive
(92/43/EEC) and Birds Directive (2009/147/EC).

Observations are not filtered by geoprivacy or research grade. If a
taxon has no legal information in EUNIS, NA values are returned for
`Legal text` and `Annex`.

## Usage

``` r
add_eunis_legal_to_occ(occ_eLTER)
```

## Arguments

- occ_eLTER:

  A `tibble` containing iNaturalist occurrences. Must contain a column
  `taxon.id`.

## Value

A `tibble` containing all original columns of `occ_eLTER` plus:

- `Legal text`:

  Legal directive text from EUNIS (92/43/EEC or 2009/147/EC)

- Annex:

  Annex information from EUNIS table

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
