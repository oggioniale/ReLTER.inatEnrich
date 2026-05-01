# ReLTER.inatEnrich <img src="man/figures/logo.png" align="right" height="139" alt="ReLTER" />

<!-- badges: start -->
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.19954246.svg)](https://doi.org/10.5281/zenodo.19954246)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: GPL-3](https://img.shields.io/badge/License-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![R-CMD-check](https://github.com/oggioniale/WISP.data/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/oggioniale/WISP.data/actions/workflows/R-CMD-check)
[![Codecov test coverage](https://codecov.io/gh/oggioniale/ReLTER.inatEnrich/graph/badge.svg)](https://app.codecov.io/gh/oggioniale/ReLTER.inatEnrich)
<!-- badges: end -->

## Overview

`ReLTER.inatEnrich` is a companion module for the
[ReLTER](https://github.com/ropensci/ReLTER) package. It enriches
iNaturalist biodiversity occurrence records — downloaded for a specific
[eLTER-RI](https://elter-ri.eu) site — with conservation and legal
attributes sourced from iNaturalist and the EUNIS species database.

### What it adds to your occurrence data

| Function | Data source | What it adds |
|---|---|---|
| `add_iucn_to_occ()` | iNaturalist API | IUCN Red List status per geographic scope, with links to assessments |
| `add_nativeness_to_occ()` | iNaturalist API | Nativeness / establishment means, filtered by country |
| `add_eunis_legal_to_occ()` | EUNIS species database | EU Habitats (92/43/EEC) and Birds (2009/147/EC) Directive coverage |
| `create_leaflet_occ_map()` | — | Interactive Leaflet map with enriched observation popups |


## ReLTER ecosystem

`ReLTER.inatEnrich` is part of the **ReLTER** software ecosystem,
a set of R tools developed to support data access and analysis for
[eLTER-RI](https://elter-ri.eu) — the European Long-Term Ecosystem,
Critical Zone and Socio-Ecological Research Infrastructure.

```
ReLTER                          ← core package (site info, occurrence download)
└── ReLTER.inatEnrich           ← this package (iNaturalist enrichment)
```

The core `ReLTER` package provides the functions used upstream of this
module (`get_site_speciesOccurrences()`, `get_site_info()`). See the
[ReLTER repository](https://github.com/ropensci/ReLTER) and its
[citation](https://doi.org/10.5281/zenodo.16927384) for details.

## :arrow_double_down: Installation

`ReLTER.inatEnrich` is not yet on CRAN. Install the development version
from GitHub:

```r
# install.packages("remotes")
remotes::install_github("oggioniale/ReLTER.inatEnrich")
```

The core `ReLTER` package is also required:

```r
remotes::install_github("ropensci/ReLTER")
```

### Dependencies

`ReLTER.inatEnrich` imports: `dplyr`, `purrr`, `httr2`, `sf`, `leaflet`, `ggplot2`, `rvest`, `stats`.

All dependencies are installed automatically with the package.


## Quick start

```r
library(ReLTER)
library(ReLTER.inatEnrich)

# 1. Define the eLTER-RI site by DEIMS-ID
deimsid <- "https://deims.org/6b62feb2-61bf-47e1-b97f-0e909c408db8"

# 2. Download iNaturalist occurrences for the site
iNat_occ <- get_site_speciesOccurrences(
  deimsid  = deimsid,
  list_DS  = "inat",
  show_map = FALSE,
  limit    = 5000
)

# 3. Get site boundary and country
site_boundary <- get_site_info(deimsid = deimsid)
country <- site_boundary$country

# 4. Clip to site boundary
occ_in_site <- sf::st_intersection(
  x = iNat_occ$inat,
  y = site_boundary
)

# 5. Enrich with IUCN status, nativeness, and EU legal framework
occ_enriched <- occ_in_site |>
  add_iucn_to_occ() |>
  add_nativeness_to_occ(country = country) |>
  add_eunis_legal_to_occ()

# 6. Visualise
map <- create_leaflet_occ_map(occ_enriched = occ_enriched)
map

```

[![Example map output](https://raw.githubusercontent.com/oggioniale/ReLTER.inatEnrich/main/man/figures/map_screenshot.png)](https://oggioniale.github.io/ReLTER.inatEnrich/figures/map_example.html)
> *Click the image to open an interactive map.*

For a full walkthrough see the
[Getting Started vignette](https://oggioniale.github.io/ReLTER.inatEnrich/articles/ReLTER_inatEnrich.html)
and the
[Technical Reference article](https://oggioniale.github.io/ReLTER.inatEnrich/articles/ReLTER_inatEnrich_article.html).
For an overview of all visualization outputs see the
[Visualization functions article](https://oggioniale.github.io/ReLTER.inatEnrich/articles/ReLTER_inatEnrich_charts.html).


## :book: Documentation

Full package documentation is available at:
👉 **https://oggioniale.github.io/ReLTER.inatEnrich**


## Contributing

Contributions are welcome! If you find a bug, have a feature request, or
want to improve the documentation:

- 🐛 **Bug reports** → [open an issue](https://github.com/oggioniale/ReLTER.inatEnrich/issues/new?template=bug_report.md)
- 💡 **Feature requests** → [open an issue](https://github.com/oggioniale/ReLTER.inatEnrich/issues/new?template=feature_request.md)
- 🔧 **Pull requests** → fork the repository, create a branch from `main`,
  and submit a PR describing your changes

Please follow the
[Contributor Covenant Code of Conduct](https://www.contributor-covenant.org/version/2/1/code_of_conduct/)
in all interactions.

Before submitting a PR, make sure that:

```r
devtools::check()    # no ERRORs or WARNINGs
devtools::document() # documentation is up to date
```

## :notebook_with_decorative_cover: Citation

If you use `ReLTER.inatEnrich` in your research, please cite it as:

> Oggioni A., Lenzi A., Campanaro A., Bergami C. (2026). *ReLTER.inatEnrich:
> Enriching iNaturalist occurrences for eLTER-RI sites*. R package version 0.0.0.9000.
> doi: [10.5281/zenodo.15399015](https://doi.org/10.5281/zenodo.15399015)

You can also retrieve the citation from within R:

```r
citation("ReLTER.inatEnrich")
```

Please also cite the core `ReLTER` package:

> Oggioni A. et al. (2023). *ReLTER: An Interface for the eLTER Community*.
> doi: [10.5281/zenodo.16927384](https://doi.org/10.5281/zenodo.16927384)


## License

`ReLTER.inatEnrich` is released under the
**GNU General Public License v3.0 (GPL-3)**.
See the [LICENSE](LICENSE) file for details.


## :thumbsup: Acknowledgements

This package was developed in the context of the [eLTER-RI](https://elter-ri.eu) research infrastructure.

Biodiversity data are sourced from [iNaturalist](https://www.inaturalist.org) and legal/conservation data from the
[EUNIS species database](https://eunis.eea.europa.eu) (European Environment Agency) and the [IUCN Red List](https://www.iucnredlist.org).
