#' Example iNaturalist occurrences enriched with conservation information
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A dataset of iNaturalist occurrences from the Montagna di Torricchio
#' eLTER site, enriched with IUCN conservation status, establishment means,
#' and EU directive (Habitats / Birds) legal information.
#'
#' @format An \code{sf} tibble with the following key columns:
#' \describe{
#'   \item{name}{Scientific name}
#'   \item{taxon.iconic_taxon_name}{iNaturalist iconic taxon group}
#'   \item{status_IUCN}{list-column of IUCN conservation status tibbles}
#'   \item{establishmentMeans}{list-column of establishment means tibbles}
#'   \item{directive}{character. EU directive name:
#'     \code{"Habitats Directive (92/43/EEC)"},
#'     \code{"Birds Directive (2009/147/EC)"}, or \code{NA}.}
#'   \item{annex}{character. EU directive annex (e.g. \code{"II"},
#'     \code{"I"}), or \code{NA} if not listed or annex not available.}
#' }
#' @source iNaturalist via \code{ReLTER::get_site_speciesOccurrences()},
#'   EU directive data from the BISE (Biodiversity Information System for Europe)
#'   REST API (\url{https://biodiversity.europa.eu}).
"occ_eLTER_legal"

#' eLTER site boundary for Gran Paradiso
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' An \code{sf} object representing the boundary of the Gran Paradiso
#' eLTER facility retrieved via \code{ReLTER::get_site_info()}.
#'
#' @format An \code{sf} object with site metadata and boundary polygon.
#' @source \url{https://deims.org/15c3e841-8494-42d2-a44e-c49a0ff25946}
"site_boundary"