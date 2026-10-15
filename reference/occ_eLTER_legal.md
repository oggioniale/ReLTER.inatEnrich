# Example iNaturalist occurrences enriched with conservation information

**\[experimental\]**

A dataset of iNaturalist occurrences from the Montagna di Torricchio
eLTER site, enriched with IUCN conservation status, establishment means,
and EU directive (Habitats / Birds) legal information.

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

  character. EU directive name: `"Habitats Directive (92/43/EEC)"`,
  `"Birds Directive (2009/147/EC)"`, or `NA`.

- annex:

  character. EU directive annex (e.g. `"II"`, `"I"`), or `NA` if not
  listed or annex not available.

## Source

iNaturalist via `ReLTER::get_site_speciesOccurrences()`, EU directive
data from the BISE (Biodiversity Information System for Europe) REST API
(<https://biodiversity.europa.eu>).
