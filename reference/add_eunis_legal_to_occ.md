# Enrich eLTER site iNaturalist occurrences with EU directive legal information

**\[stable\]**

Iterates over all unique `taxon.id` values in the occurrence tibble and
enriches each row with EU directive information retrieved from the
[BISE](https://biodiversity.europa.eu) (Biodiversity Information System
for Europe) REST API via
[`get_eunis_legal_info`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/get_eunis_legal_info.md).

For each taxon the function appends the directive name and the Annex
identifier (II, IV, V for Habitats; I for Birds). When a species is
listed under both directives the occurrence tibble will contain multiple
rows for that taxon – one per directive/annex entry.

## Usage

``` r
add_eunis_legal_to_occ(occ_eLTER)
```

## Arguments

- occ_eLTER:

  An `sf` tibble of iNaturalist occurrences, typically output of
  `ReLTER::get_site_speciesOccurrences()` after
  [`add_iucn_to_occ`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_iucn_to_occ.md)
  and
  [`add_nativeness_to_occ`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_nativeness_to_occ.md).

## Value

The input tibble enriched with:

- directive:

  EU directive name (`"Habitats Directive (92/43/EEC)"`,
  `"Birds Directive (2009/147/EC)"`), or `NA`.

- annex:

  Short annex code (e.g. `"II"`, `"IV"`, `"I"`), or `NA`.

## Note

EU directive data are retrieved live from the BISE REST API
(<https://biodiversity.europa.eu>) using two POST requests per unique
taxon. An internet connection is required. The function includes a short
delay between taxa (`Sys.sleep(0.2)`) to avoid overloading the server.

## See also

[`get_eunis_legal_info`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/get_eunis_legal_info.md),
[`obs_pie_chart`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/obs_pie_chart.md)

## Author

Alessandro Oggioni, PhD <alessandro.oggioni@cnr.it>

## Examples

``` r
if (FALSE) { # \dontrun{
deimsid       <- "https://deims.org/6b62feb2-61bf-47e1-b97f-0e909c408db8"
site_boundary <- ReLTER::get_site_info(deimsid = deimsid)

iNat_occ <- ReLTER::get_site_speciesOccurrences(
  deimsid  = deimsid,
  list_DS  = "inat",
  show_map = TRUE,
  limit    = 50
)

occ_legal <- iNat_occ$inat |>
  add_iucn_to_occ() |>
  add_nativeness_to_occ(country = site_boundary$country) |>
  add_eunis_legal_to_occ()
} # }
```
