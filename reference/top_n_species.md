# Plot top N most observed species

**\[experimental\]**

Takes a tibble of iNaturalist occurrences and produces a horizontal bar
chart of the top `n` most observed species, coloured by iconic taxon
group. If fewer than `n` species have more than one observation, all
species are shown.

## Usage

``` r
top_n_species(df, n = 10)
```

## Arguments

- df:

  An `sf` tibble of iNaturalist occurrences containing at least the
  columns `name` and `taxon.iconic_taxon_name`.

- n:

  Integer. Number of top species to display. Default is `10`.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) object
printed to the active device.

## See also

[`iconic_taxa`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/iconic_taxa.md),
[`obs_per_year`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/obs_per_year.md)

## Author

Alessandro Oggioni, PhD <alessandro.oggioni@cnr.it>

Alice Lenzi, phD <alice.lenzi@crea.gov.it>

## Examples

``` r
if (FALSE) { # \dontrun{
top_n_species(occ_eLTER_legal)
top_n_species(occ_eLTER_legal, n = 20)
} # }
```
