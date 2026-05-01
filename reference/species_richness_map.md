# Map species richness on a spatial grid

**\[experimental\]**

Takes a spatial tibble of iNaturalist occurrences and produces an
interactive Leaflet map where each grid cell is coloured according to
the number of unique species observed within it (species richness).
Empty cells are removed from the map.

## Usage

``` r
species_richness_map(df, site_boundary = NULL, cell_size = 0.001)
```

## Arguments

- df:

  An `sf` tibble of iNaturalist occurrences containing at least the
  column `name` (scientific name) and valid point geometry.

- site_boundary:

  An `sf` object representing the eLTER site boundary polygon. If `NULL`
  (default), no boundary is drawn.

- cell_size:

  Numeric. Size of each grid cell in decimal degrees. Default is `0.001`
  (approximately 100 m). Use larger values (e.g. `0.005`) for broader
  spatial extents.

## Value

A [`leaflet`](https://rstudio.github.io/leaflet/reference/leaflet.html)
map object with:

- An OpenStreetMap base tile layer.

- Grid cells coloured by species richness using the `"viridis"` palette.

- A popup per cell showing the species richness value.

- A legend for species richness.

- An optional eLTER site boundary polygon layer.

- A layers control to toggle map groups.

## See also

[`create_leaflet_occ_map`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/create_leaflet_occ_map.md)

## Author

Alessandro Oggioni, PhD <alessandro.oggioni@cnr.it>

Alice Lenzi, phD <alice.lenzi@crea.gov.it>

## Examples

``` r
if (FALSE) { # \dontrun{
deimsid <- "https://deims.org/6b62feb2-61bf-47e1-b97f-0e909c408db8"
site_boundary <- ReLTER::get_site_info(deimsid = deimsid)

species_richness_map(
  df            = occ_eLTER_legal,
  site_boundary = site_boundary,
  cell_size     = 0.005
)
} # }
```
