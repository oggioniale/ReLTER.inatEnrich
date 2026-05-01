# Create Leaflet map for enriched iNaturalist occurrences

**\[experimental\]**

Takes a tibble of iNaturalist occurrences already enriched with species
nativeness, IUCN conservation status, and EUNIS legal information.
Verifies that all required columns are present, builds observation-level
HTML popups with linked references to iNaturalist, IUCN Red List, and
EUR-Lex directive pages, and returns an interactive Leaflet map with
colour-coded markers by iconic taxon group and marker clustering.

## Usage

``` r
create_leaflet_occ_map(occ_enriched, site_boundary = NULL)
```

## Arguments

- occ_enriched:

  An `sf` tibble of iNaturalist occurrences enriched with the following
  columns:

  establishmentMeans

  :   list-column of 1 × 2 tibbles with `nativeness` and `authority`,
      produced by
      [`add_nativeness_to_occ`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_nativeness_to_occ.md).

  status_IUCN

  :   list-column of N × 4 tibbles with `status`, `authority`, `name`
      (geographic scope), and `url`, produced by
      [`add_iucn_to_occ`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_iucn_to_occ.md).

  directive

  :   character. EU directive name, produced by
      [`add_eunis_legal_to_occ`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_eunis_legal_to_occ.md).

  annex

  :   character. EU directive annex, produced by
      [`add_eunis_legal_to_occ`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_eunis_legal_to_occ.md).

  taxon.id

  :   integer. iNaturalist taxon ID.

  name

  :   character. Scientific name.

  taxon.preferred_common_name

  :   character. Common name.

  taxon.iconic_taxon_name

  :   character. Iconic taxon group (e.g. `"Aves"`, `"Plantae"`) used
      for marker colouring.

  taxon_geoprivacy

  :   character. Geoprivacy status of the taxon (`"open"`, `"obscured"`,
      `"private"`).

  quality_grade

  :   character. iNaturalist data quality grade.

  observed_on

  :   Date. Observation date.

  public_positional_accuracy

  :   numeric. Positional accuracy in metres.

  user.login

  :   character. iNaturalist username of the observer.

  uri

  :   character. URL of the iNaturalist observation record.

  taxon.default_photo.square_url

  :   character. URL of the taxon thumbnail photo shown in the popup.

## Value

A [`leaflet`](https://rstudio.github.io/leaflet/reference/leaflet.html)
map object with:

- An OpenStreetMap base tile layer.

- The eLTER-RI site boundary polygon (retrieved via
  `ReLTER::get_site_info()`).

- Circle markers for `"open"` observations (radius 6, full opacity) and
  `"obscured"` observations (radius 10, reduced opacity), both with
  marker clustering enabled.

- Popups per observation showing taxon info, observer details, data
  quality, IUCN status per geographic scope (with links to IUCN Red List
  assessments), establishment means (with link to iNaturalist help), and
  EU directive coverage (with links to EUR-Lex).

- A legend for iconic taxon groups.

- A geoprivacy legend control (top right).

- A layers control to toggle `"open"` and `"obscured"` marker groups.

## Note

The function calls `ReLTER::get_site_info()` internally to retrieve the
site boundary, using the DEIMS-ID stored in `occ_enriched$uri.1[1]`.
Observations with `taxon_geoprivacy == "private"` are silently excluded
from the map as they have no displayable coordinates.

EU directive links point to the EUR-Lex PDF versions: Birds Directive
2009/147/EC and Habitats Directive 92/43/EEC.

## See also

[`add_iucn_to_occ`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_iucn_to_occ.md),
[`add_nativeness_to_occ`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_nativeness_to_occ.md),
[`add_eunis_legal_to_occ`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_eunis_legal_to_occ.md)

## Author

Alessandro Oggioni, PhD <alessandro.oggioni@cnr.it>

## Examples

``` r
if (FALSE) { # \dontrun{
deimsid <- "https://deims.org/6b62feb2-61bf-47e1-b97f-0e909c408db8"
site_boundary <- ReLTER::get_site_info(deimsid = deimsid)

map <- create_leaflet_occ_map(
  occ_enriched = occ_eLTER_legal,
  site_boundary = site_boundary
)
map

} # }
```
