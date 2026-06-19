# Plot a donut chart of species by conservation category

**\[experimental\]**

Takes a tibble of iNaturalist occurrences enriched with establishment
means and EU directive information, and produces a donut chart showing
the proportion of species assigned to each conservation category. Each
species is assigned to exactly one category following a fixed priority
order: Alien (IAS) \> Habitats Directive \> Birds Directive \> Other.

## Usage

``` r
obs_pie_chart(df)
```

## Arguments

- df:

  An `sf` tibble of iNaturalist occurrences containing at least the
  columns:

  name

  :   character. Scientific name.

  directive

  :   character. EU directive name, produced by
      [`add_eunis_legal_to_occ`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_eunis_legal_to_occ.md).

  establishmentMeans

  :   list-column of tibbles with a `iNat_nativeness` field, produced by
      [`add_nativeness_to_occ`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_nativeness_to_occ.md).

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) object
printed to the active device. The total number of species is displayed
at the centre of the donut. Category counts and percentages are shown in
the legend.

## Note

Species are assigned to one category only, in order of priority:

1.  Alien (IAS) — `nativeness == "introduced"`

2.  Habitats Directive — `directive` contains `"Habitats"`

3.  Birds Directive — `directive` contains `"Birds"`

4.  Other — all remaining species

A species meeting multiple criteria will appear only in the
highest-priority category.

## See also

[`add_nativeness_to_occ`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_nativeness_to_occ.md),
[`add_eunis_legal_to_occ`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_eunis_legal_to_occ.md)

## Author

Alessandro Oggioni, PhD <alessandro.oggioni@cnr.it>

Alice Lenzi, phD <alice.lenzi@crea.gov.it>

## Examples

``` r
if (FALSE) { # \dontrun{
#' # Download taxa occurrences from iNaturalist using ReLTER's
# get_site_speciesOccurrences() function
# e.g. Montagna di Torricchio eLTER site
deimsid <- "https://deims.org/6b62feb2-61bf-47e1-b97f-0e909c408db8"
site_boundary <- ReLTER::get_site_info(deimsid = deimsid)

iNat_occ_eLTER_site <- ReLTER::get_site_speciesOccurrences(
  deimsid = deimsid,
  list_DS = "inat",
  show_map = TRUE,
  limit = 50
)
occ_eLTER_enrich <- add_iucn_to_occ(
  occ_eLTER = iNat_occ_eLTER_site$inat
) |>
  add_nativeness_to_occ(
    country = site_boundary$country
  ) |>
  add_eunis_legal_to_occ()
  
obs_pie_chart(occ_eLTER_enrich)
} # }
```
