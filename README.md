# ReLTER.inatEnrich <img src="man/figures/logo.png" align="right" height="139" alt="ReLTER" />

<!-- badges: start -->
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.19954245.svg)](https://doi.org/10.5281/zenodo.19954245)
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
attributes sourced from iNaturalist EASIN and the EUNIS species database.

### What it adds to your occurrence data

| Function | Data source | What it adds |
|---|---|---|
| `add_iucn_to_occ()` | iNaturalist API | IUCN Red List status per geographic scope, with links to assessments, sourced from [iNaturalist checklists](https://forum.inaturalist.org/t/updating-iucn-red-list-conservation-statuses/25712) |
| `add_nativeness_to_occ()` | iNaturalist API | Nativeness / [establishment means](https://dwc.tdwg.org/em/), filtered by country, directly as recorded on [iNaturalist](https://help.inaturalist.org/en/support/solutions/articles/151000176171-how-to-add-or-edit-establishment-means-in-inaturalist) |
| `add_nativeness_to_occ()` | EASIN API | IAS information from EASIN - European Alien Species Information Network |
| `add_eunis_legal_to_occ()` | EUNIS species database | EU Habitats (92/43/EEC) and Birds (2009/147/EC) Directive coverage, as delivered by retrieved from the [EUNIS](https://eunis.eea.europa.eu/) species database |
| all three functions above | iNaturalist API (taxonomic ancestry) | Automatic assignment of [eLTER Standard Observations](https://elter-ri.eu/standard-observations-spheres) ([`SOBIO_014` Flying insects](https://elter-ri.eu/standard-observations-spheres/biosphere), [`SOBIO_017` Vegetation composition](https://elter-ri.eu/standard-observations-spheres/biosphere), [`SOBIO_018` Acoustic recording](https://elter-ri.eu/standard-observations-spheres/biosphere)) based on taxonomic ancestry — performed once at the first enrichment step and skipped thereafter |
| Visualization functions | — | Charts and interactive maps for enriched occurrence data — see [Visualization functions](https://oggioniale.github.io/ReLTER.inatEnrich/articles/ReLTER_inatEnrich_charts.html) |

### Disclaimer
_`ReLTER.inatEnrich` is designed to standardize iNaturalist data for consistent and comparable analysis. While methodological accuracy and interoperability of the results are ensured, the authors disclaim any responsibility for the quality or accuracy of the raw input data, as the functions rely entirely on variables and values provided by iNaturalist original source and other official international databases (i.e., EUNIS and EASIN). Given this, users are strongly advised to perform their own data quality checks and validation, paying particular attention to the fields related to Establishment Means and IUCN status.


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

## Installation

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

`ReLTER.inatEnrich` imports: `ReLTER`, `dplyr`, `purrr`, `httr2`, `sf`, `leaflet`, `ggplot2`, `rvest`, `stats`.

All dependencies are installed automatically with the package.


## Package workflow

put here the picture of WF ...

<a href="https://oggioniale.github.io/ReLTER.inatEnrich/figures/map_example.html">
  <img src="https://raw.githubusercontent.com/oggioniale/ReLTER.inatEnrich/main/man/figures/map_screenshot.png" 
       alt="Example map output" 
       width="60%"/>
</a>

*Click the image to open an interactive map.*

For an overview of all visualization outputs see the
[Visualization functions article](https://oggioniale.github.io/ReLTER.inatEnrich/articles/ReLTER_inatEnrich_charts.html).

For a full walkthrough see the
[Getting Started vignette](https://oggioniale.github.io/ReLTER.inatEnrich/articles/ReLTER_inatEnrich.html)
and the
[Technical Reference article](https://oggioniale.github.io/ReLTER.inatEnrich/articles/ReLTER_inatEnrich_article.html).


## Documentation

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

## Citation

If you use `ReLTER.inatEnrich` in your research, please cite it as:

> Oggioni A., Lenzi A., Campanaro A., Bergami C. (2026). *ReLTER.inatEnrich:
> Enriching iNaturalist occurrences for eLTER-RI sites*. R package version 1.1.0.
> doi: [10.5281/zenodo.19954245](https://doi.org/10.5281/zenodo.19954245)

You can also retrieve the citation from within R:

```r
citation("ReLTER.inatEnrich")
```


## License

`ReLTER.inatEnrich` is released under the
**GNU General Public License v3.0 (GPL-3)**.
See the [LICENSE](LICENSE) file for details.


## Acknowledgements

This package was developed in the context of the [eLTER-RI](https://elter-ri.eu) research infrastructure.

Biodiversity data are sourced from [iNaturalist](https://www.inaturalist.org) and legal/conservation data from the
[EUNIS species database](https://eunis.eea.europa.eu) (European Environment Agency) and the [IUCN Red List](https://www.iucnredlist.org).
