# Plot number of observations per year

**\[experimental\]**

Takes a tibble of iNaturalist occurrences and produces a stacked bar
chart of observations per year, coloured by iconic taxon group. All
years from the first observation to the current year are shown,
including years with zero observations.

## Usage

``` r
obs_per_year(df)
```

## Arguments

- df:

  An `sf` tibble of iNaturalist occurrences containing at least the
  columns `observed_on_details.year` and `taxon.iconic_taxon_name`.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) object
printed to the active device.

## See also

[`obs_per_month`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/obs_per_month.md),
[`obs_per_hour`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/obs_per_hour.md)

## Author

Alessandro Oggioni, PhD <alessandro.oggioni@cnr.it>

Alice Lenzi, phD <alice.lenzi@crea.gov.it>

## Examples

``` r
if (FALSE) { # \dontrun{
obs_per_year(occ_eLTER_legal)
} # }
```
