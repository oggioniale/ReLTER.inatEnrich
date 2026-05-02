# Example iNaturalist occurrences enriched with conservation information

**\[experimental\]**

A dataset of iNaturalist occurrences from the Montagna di Torricchio
eLTER site, enriched with IUCN conservation status, establishment means,
and EUNIS legal information.

## Usage

``` r
occ_eLTER_legal
```

## Format

An `sf` tibble with the following key columns:

- name:

  Scientific name

- taxon.iconic_taxon_name:

  iNaturalist iconic taxon group

- status_IUCN:

  list-column of IUCN conservation status tibbles

- establishmentMeans:

  list-column of establishment means tibbles

- directive:

  EU directive name

- annex:

  EU directive annex

## Source

iNaturalist via `ReLTER::get_site_speciesOccurrences()`
