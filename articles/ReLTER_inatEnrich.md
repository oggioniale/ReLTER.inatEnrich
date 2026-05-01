# Enriching iNaturalist Occurrences for eLTER-RI Sites

## Introduction

`ReLTER.inatEnrich` is a companion module for the
[ReLTER](https://doi.org/10.5281/zenodo.16927384) package. It extends
the core ReLTER functionality by enriching iNaturalist biodiversity
occurrence records — downloaded for a specific eLTER-RI site — with a
set of conservation and legal attributes:

- **Nativeness / establishment means** (native, introduced, etc.)
  sourced from iNaturalist checklists, filtered by country
- **IUCN Red List conservation status** at global and regional level,
  directly as recorded on iNaturalist
- **EU legal framework** coverage under the Habitats Directive
  (92/43/EEC) and the Birds Directive (2009/147/EC), retrieved from the
  [EUNIS](https://eunis.eea.europa.eu/) species database

The final output is a fully enriched occurrence dataset and an
interactive Leaflet map summarising all information in observation-level
popups.

------------------------------------------------------------------------

## Requirements

The following packages must be installed before using
`ReLTER.inatEnrich`:

``` r

# Core dependencies
install.packages(c(
  "dplyr",
  "purrr",
  "httr2",
  "sf",
  "leaflet"
))

# ReLTER — the parent package providing site info and occurrence download
# https://doi.org/10.5281/zenodo.16927384
# install.packages("remotes")
remotes::install_github("eLTER-RI/ReLTER")

# ReLTER.inatEnrich — this package
remotes::install_github("eLTER-RI/ReLTER.inatEnrich")
```

Then load the packages:

``` r

library(ReLTER)
library(ReLTER.inatEnrich)
```

------------------------------------------------------------------------

## What is a DEIMS-ID?

The **DEIMS-ID** (Dynamic Ecological Information Management System –
Site and dataset registry ID) is a unique and persistent identifier
assigned to each site registered in the [DEIMS-SDR](https://deims.org)
registry — the central catalogue of eLTER-RI research sites.

A DEIMS-ID takes the form of a resolvable URL, for example:

    https://deims.org/6b62feb2-61bf-47e1-b97f-0e909c408db8

You can browse registered sites at [deims.org](https://deims.org) and
copy the DEIMS-ID from the site page. It is the entry point for all
ReLTER functions: it allows the package to retrieve site boundaries,
country information, and linked datasets from a single, unambiguous
identifier.

For more information see the [DEIMS-ID
documentation](https://deims.org/docs/deimsid.html).

------------------------------------------------------------------------

## Workflow

The enrichment pipeline consists of eight steps, illustrated below.

### Step 1 — Define the eLTER-RI site

Set the DEIMS-ID of the target eLTER-RI site. In this example we use the
**Bosco Fontana** site (Italy).

``` r

deimsid <- "https://deims.org/6b62feb2-61bf-47e1-b97f-0e909c408db8"
```

### Step 2 — Download iNaturalist occurrences

Download all iNaturalist observations associated with the eLTER-RI site
using `ReLTER::get_site_speciesOccurrences()`. The `limit` argument caps
the maximum number of records returned.

``` r

iNat_occ_eLTER_site <- ReLTER::get_site_speciesOccurrences(
  deimsid  = deimsid,
  list_DS  = "inat",
  show_map = TRUE,
  limit    = 5000
)
```

### Step 3 — Retrieve site boundary and country

Download the site boundary polygon and extract the country name. The
country is used in the following step to filter establishment means to
the correct national checklist.

``` r

site_boundary <- ReLTER::get_site_info(deimsid = deimsid)
country       <- site_boundary$country
```

### Step 4 — Clip occurrences to the site boundary

Use a spatial intersection to retain only the observations that fall
within the site boundary polygon.

``` r

occ_in_site <- sf::st_intersection(
  x = iNat_occ_eLTER_site$inat,
  y = site_boundary
)
```

### Step 5 — Add IUCN conservation status

[`add_iucn_to_occ()`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_iucn_to_occ.md)
queries iNaturalist for each unique taxon and appends a nested
`status_IUCN` list-column. Each element is a tibble with one row per
geographic scope (global, regional), containing the IUCN status code,
authority, scope name, and a direct link to the IUCN Red List
assessment.

``` r

occ_eLTER_IUCN <- add_iucn_to_occ(occ_eLTER = occ_in_site)
```

    # Example content of status_IUCN[[1]]:
    # A tibble: 3 × 4
    #   status authority      name          url
    #   <chr>  <chr>          <chr>         <chr>
    # 1 LC     IUCN Red List  NA            https://www.iucnredlist.org/...
    # 2 LC     IUCN Red List  Europe        https://www.iucnredlist.org/...
    # 3 LC     IUCN Red List  Mediterranean https://www.iucnredlist.org/...

### Step 6 — Add nativeness / establishment means

[`add_nativeness_to_occ()`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_nativeness_to_occ.md)
queries iNaturalist for the establishment means of each unique taxon,
filtered by the site’s country. Results are stored in a nested
`establishmentMeans` list-column containing `nativeness` and
`authority`.

``` r

occ_eLTER_nativeness <- add_nativeness_to_occ(
  occ_eLTER = occ_eLTER_IUCN,
  country   = country
)
```

    # Example content of establishmentMeans[[1]]:
    # A tibble: 1 × 2
    #   nativeness authority
    #   <chr>      <chr>
    # 1 native     Italy Check List

### Step 7 — Add EU legal framework (EUNIS)

[`add_eunis_legal_to_occ()`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/add_eunis_legal_to_occ.md)
queries the EUNIS species database and appends `directive` and `Annex`
columns indicating coverage under the EU Habitats Directive (92/43/EEC)
or the EU Birds Directive (2009/147/EC).

``` r

occ_eLTER_legal <- add_eunis_legal_to_occ(occ_eLTER = occ_eLTER_nativeness)
```

### Step 8 — Create the interactive map

[`create_leaflet_occ_map()`](https://oggioniale.github.io/ReLTER.inatEnrich/reference/create_leaflet_occ_map.md)
produces an interactive Leaflet map with colour-coded markers by iconic
taxon group, clustering, and rich popups showing all enriched attributes
for each observation.

``` r

map <- create_leaflet_occ_map(occ_enriched = occ_eLTER_legal)
map
```

The popup for each observation includes:

- Taxon name (linked to iNaturalist) and common name
- Observer and direct link to the observation record
- Observation quality grade, geoprivacy, and positional accuracy
- IUCN status per geographic scope, with links to Red List assessments
- Nativeness and authority (from iNaturalist national checklist)
- EU Directive coverage and relevant Annex, one row per directive

------------------------------------------------------------------------

## Output example

> **Note:** The map below is a static screenshot. Run the workflow above
> in your R session to obtain the fully interactive version.

------------------------------------------------------------------------

## Session info

``` r

sessionInfo()
#> R version 4.6.0 (2026-04-24)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.4 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> loaded via a namespace (and not attached):
#>  [1] digest_0.6.39     desc_1.4.3        R6_2.6.1          fastmap_1.2.0    
#>  [5] xfun_0.57         cachem_1.1.0      knitr_1.51        htmltools_0.5.9  
#>  [9] rmarkdown_2.31    lifecycle_1.0.5   cli_3.6.6         sass_0.4.10      
#> [13] pkgdown_2.2.0     textshaping_1.0.5 jquerylib_0.1.4   systemfonts_1.3.2
#> [17] compiler_4.6.0    tools_4.6.0       ragg_1.5.2        bslib_0.10.0     
#> [21] evaluate_1.0.5    yaml_2.3.12       jsonlite_2.0.0    rlang_1.2.0      
#> [25] fs_2.1.0          htmlwidgets_1.6.4
```
