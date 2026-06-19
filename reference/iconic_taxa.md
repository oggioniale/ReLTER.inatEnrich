# Plot number of observations and species per iconic taxonomic group

**\[experimental\]**

Takes a tibble of iNaturalist occurrences and produces a grouped bar
chart showing the total number of observations and unique species for
each iconic taxonomic group as defined by iNaturalist.

## Usage

``` r
iconic_taxa(df)
```

## Arguments

- df:

  An `sf` tibble of iNaturalist occurrences containing at least the
  columns `taxon.iconic_taxon_name` and `name`.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) object
printed to the active device.

## See also

[`top_n_species`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/top_n_species.md),
[`obs_per_year`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/obs_per_year.md)

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
  
iconic_taxa(occ_eLTER_enrich)
} # }
```
