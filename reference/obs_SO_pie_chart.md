# Plot a donut chart of observations contributing to eLTER Standard Observations

**\[experimental\]**

Produces a donut chart showing the number of observations contributing
to eLTER Standard Observations SOBIO_014 (Flying insects) and SOBIO_018
(Acoustic recording). Observations can contribute to both SOs
simultaneously (e.g. Orthoptera).

## Usage

``` r
obs_SO_pie_chart(df)
```

## Arguments

- df:

  An `sf` tibble of iNaturalist occurrences containing at least the
  columns `SOBIO_014` and `SOBIO_018`, produced by the enrichment
  pipeline.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) object
printed to the active device.

## See also

[`obs_pie_chart`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/obs_pie_chart.md)

## Author

Alessandro Oggioni, PhD <alessandro.oggioni@cnr.it>

Alice Lenzi, phD <alice.lenzi@crea.gov.it>

## Examples

``` r
if (FALSE) { # \dontrun{
obs_SO_pie_chart(occ_eLTER_legal)
} # }
```
