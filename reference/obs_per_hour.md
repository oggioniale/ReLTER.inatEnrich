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
obs_per_hour(occ_eLTER_legal)
} # }
```
