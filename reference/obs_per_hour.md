# Plot mean number of observations per hour of the day

**\[experimental\]**

Takes a tibble of iNaturalist occurrences and produces a stacked bar
chart of mean observations per hour of the day across years, coloured by
iconic taxon group. Error bars show ± 1 standard error of the mean
across years. All 24 hours are always shown, including those with zero
observations.

## Usage

``` r
obs_per_hour(df)
```

## Arguments

- df:

  An `sf` tibble of iNaturalist occurrences containing at least the
  columns `observed_on_details.hour`, `observed_on_details.year`, and
  `taxon.iconic_taxon_name`.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) object
printed to the active device.

## See also

[`obs_per_year`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/obs_per_year.md),
[`obs_per_month`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/obs_per_month.md)

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
  
obs_per_hour(occ_eLTER_enrich)
} # }
```
