#' Example iNaturalist occurrences enriched with conservation information
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A dataset of iNaturalist occurrences from the Montagna di Torricchio
#' eLTER site, enriched with IUCN conservation status, establishment means,
#' and EUNIS legal information.
#'
#' @format An \code{sf} tibble with the following key columns:
#' \describe{
#'   \item{name}{Scientific name}
#'   \item{taxon.iconic_taxon_name}{iNaturalist iconic taxon group}
#'   \item{status_IUCN}{list-column of IUCN conservation status tibbles}
#'   \item{establishmentMeans}{list-column of establishment means tibbles}
#'   \item{directive}{EU directive name}
#'   \item{annex}{EU directive annex}
#' }
#' @source iNaturalist via \code{ReLTER::get_site_speciesOccurrences()}
"occ_eLTER_legal"


#' eLTER site boundary for Montagna di Torricchio
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' An \code{sf} object representing the boundary of the Montagna di
#' Torricchio eLTER site retrieved via \code{ReLTER::get_site_info()}.
#'
#' @format An \code{sf} object with site metadata and boundary polygon.
#' @source \url{https://deims.org/6b62feb2-61bf-47e1-b97f-0e909c408db8}
"site_boundary"