#' @keywords internal
#' @author Alessandro Oggioni, PhD \email{alessandro.oggioni@@cnr.it}
#' @noRd
.assign_eLTER_SOs <- function(occ) {
  
  ANURA_ID <- 20979
  CHIROPTERA_ID <- 40268
  ORTHOPTERA_ID <- 47651
  AVES_ID <- 3
  INSECTA_ID <- 47158
  PLANTS_ID <- 47126
  
  has_ancestor <- function(ancestor_ids, target_id) {
    if (is.null(ancestor_ids) || length(ancestor_ids) == 0) return(FALSE)
    target_id %in% unlist(ancestor_ids)
  }
  
  occ |>
    dplyr::mutate(
      .is_aves = vapply(taxon.ancestor_ids,
                           function(x) has_ancestor(x, AVES_ID),
                           FUN.VALUE = logical(1)),
      .is_insecta = vapply(taxon.ancestor_ids,
               function(x) has_ancestor(x, INSECTA_ID),
               FUN.VALUE = logical(1)),
      .is_anura = vapply(taxon.ancestor_ids,
                              function(x) has_ancestor(x, ANURA_ID),
                              FUN.VALUE = logical(1)),
      .is_chiroptera = vapply(taxon.ancestor_ids,
                              function(x) has_ancestor(x, CHIROPTERA_ID),
                              FUN.VALUE = logical(1)),
      .is_orthoptera = vapply(taxon.ancestor_ids,
                              function(x) has_ancestor(x, ORTHOPTERA_ID),
                              FUN.VALUE = logical(1)),
      .is_plants = vapply(taxon.ancestor_ids,
                              function(x) has_ancestor(x, PLANTS_ID),
                              FUN.VALUE = logical(1)),
      SOBIO_018 = .is_aves | .is_anura | .is_chiroptera | .is_orthoptera,
      SOBIO_014 = .is_insecta,
      SOBIO_017 = .is_plants
    ) |>
    dplyr::select(-.is_aves, -.is_insecta, -.is_anura,
                  -.is_chiroptera, -.is_orthoptera, -.is_plants)
}

#' @keywords internal
#' @author Alessandro Oggioni, PhD \email{alessandro.oggioni@@cnr.it}
#' @noRd
#' @examples
#' \dontrun{
#' ## Not run:
#' # https://easin.jrc.ec.europa.eu/apixg/catxg/term/Falco%20tinnunculus
#' .assign_EASIN_info(specie_name = "Falco tinnunculus")
#' 
#' # https://easin.jrc.ec.europa.eu/apixg/catxg/term/Vespa%20velutina%20nigrithorax
#' .assign_EASIN_info(specie_name = "Vespa velutina")
#' }
#' 
.assign_EASIN_info <- function(specie_name) {
  easin_empty_tbl <- dplyr::tibble(
    EASIN_url = NA_character_,
    EASIN_id = NA_character_,
    EASIN_LSID = NA_character_,
    EASIN_firstIntroductionsInEU_year = NA_character_,
    EASIN_firstIntroductions_Country = NA_character_,
    EASIN_status = NA_character_,
    EASIN_hasImpact = NA_character_,
    EASIN_IsEUConcern = NA_character_,
  )
  
  easin_api_url <- paste0(
    'https://easin.jrc.ec.europa.eu/apixg/catxg/term/',
    URLencode(specie_name)
  )
  
  easin_response <- tryCatch({
    httr2::request(easin_api_url) |> 
      httr2::req_method("GET") |> 
      httr2::req_headers(Accept = "application/json") |> 
      httr2::req_retry(max_tries = 3, max_seconds = 120) |> 
      httr2::req_perform()
  }, error = function(e) {
    return(NULL)
  })
  
  # check easin_response
  if (is.null(easin_response) || httr2::resp_status(easin_response) != 200) {
    return(easin_empty_tbl)
  }
  
  # parse JSON
  easin_data <- tryCatch({
    httr2::resp_body_json(easin_response, simplifyVector = TRUE)
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(easin_data) || length(easin_data) == 0 || !is.null(easin_data$Empty)) {
    return(easin_empty_tbl)
  }
  
  .safe_intro_field <- function(intro, field) {
    if (is.null(intro) || length(intro) == 0) return(NA_character_)
    if (!is.list(intro)) return(NA_character_)
    first <- intro[[1]]
    if (!is.list(first)) return(NA_character_)   # <-- blocca se intro[[1]] è atomico
    val <- first[[field]]
    if (is.null(val) || length(val) == 0) return(NA_character_)
    as.character(val)
  }
  
  return(tibble::tibble(
    EASIN_url  = paste0(
      "https://easin.jrc.ec.europa.eu/spexplorer/species/factsheet/",
      easin_data$EASINID
    ),
    EASIN_id   = as.character(easin_data$EASINID),
    EASIN_LSID = paste0(
      "urn:lsid:easin.jrc.ec.europa.eu:species:",
      easin_data$EASINID
    ),
    EASIN_firstIntroductionsInEU_year = .safe_intro_field(
      easin_data$FirstIntroductionsInEU, "Year"
    ),
    EASIN_firstIntroductions_Country  = .safe_intro_field(
      easin_data$FirstIntroductionsInEU, "Country"
    ),
    EASIN_status      = as.character(easin_data$Status     %||% NA_character_),
    EASIN_hasImpact   = as.character(easin_data$HasImpact  %||% NA_character_),
    EASIN_IsEUConcern = as.character(easin_data$IsEUConcern %||% NA_character_)
  ))
}

#' @keywords internal
#' @author Alessandro Oggioni, PhD \email{alessandro.oggioni@@cnr.it}
#' @noRd
#' @examples
#' \dontrun{
#' ## Not run:
#' .country_to_flag(country = "Italy")
#' }
.country_to_flag <- function(country) {
  
  country_codes <- c(
    "Albania" = "AL", "Andorra" = "AD", "Austria" = "AT",
    "Belarus" = "BY", "Belgium" = "BE", "Bosnia and Herzegovina" = "BA",
    "Bulgaria" = "BG", "Croatia" = "HR", "Cyprus" = "CY",
    "Czech Republic" = "CZ", "Czechia" = "CZ", "Denmark" = "DK",
    "Estonia" = "EE", "Finland" = "FI", "France" = "FR",
    "Germany" = "DE", "Greece" = "GR", "Hungary" = "HU",
    "Iceland" = "IS", "Ireland" = "IE", "Italy" = "IT",
    "Kosovo" = "XK", "Latvia" = "LV", "Liechtenstein" = "LI",
    "Lithuania" = "LT", "Luxembourg" = "LU", "Malta" = "MT",
    "Moldova" = "MD", "Monaco" = "MC", "Montenegro" = "ME",
    "Netherlands" = "NL", "North Macedonia" = "MK", "Norway" = "NO",
    "Poland" = "PL", "Portugal" = "PT", "Romania" = "RO",
    "Russia" = "RU", "San Marino" = "SM", "Serbia" = "RS",
    "Slovakia" = "SK", "Slovenia" = "SI", "Spain" = "ES",
    "Sweden" = "SE", "Switzerland" = "CH", "Turkey" = "TR",
    "Ukraine" = "UA", "United Kingdom" = "GB", "Vatican" = "VA"
  )
  
  if (is.na(country) || country == "-") return("-")
  
  country <- as.character(country)
  
  if (nchar(country) == 2 && grepl("^[A-Z]{2}$", toupper(country))) {
    code <- toupper(country)
    country_name <- names(country_codes)[country_codes == code][1]
    if (is.na(country_name)) country_name <- code
  } else {
    code <- country_codes[country]
    if (is.na(code)) return(country)
    country_name <- country
  }
  
  # costruzione flag
  chars <- strsplit(code, "")[[1]]
  flag <- paste0(
    vapply(chars, function(ch) {
      intToUtf8(utf8ToInt("\U0001F1E6") + utf8ToInt(ch) - utf8ToInt("A"))
    }, character(1)),
    collapse = ""
  )
  
  paste0(flag, " ", country_name)
}

#' Function to obtain IUCN conservation status for a single taxon.id from
#' iNaturalist API
#' @description `r lifecycle::badge("stable")`
#' This function generates a request to the iNaturalist API of a given
#' `taxon.id` to obtain, if updated, the IUCN conservation status, where
#' the authority is the "IUCN Red List".
#' 
#' Registered user can update iNaturalist IUCN conservation status.
#' For more information about the procedure for updating the IUCN conservation
#' status in iNaturalist community, please refer to the following link:
#' https://forum.inaturalist.org/t/updating-iucn-red-list-conservation-statuses/25712
#' 
#' For more information about iNaturalist API of Taxa, please refer to the
#' following link: https://api.inaturalist.org/v1/docs/#!/Taxa/get_taxa_id
#' using as "id" value 517449. The JSON key that contains info about IUCN
#' conservation staus is 'conservation_statuses[]'.
#' @param taxon.id A `number` value representing the taxon.id of the taxa in
#' iNaturalist database.
#' @return A `tibble` containing the conservation status information of the taxa
#' retrieved from iNaturalist. The tibble includes the columns `status`,
#' `authority`, `name`, and `url`. If the parsing fails or no IUCN Red List
#' conservation status is available, the function returns a tibble with
#' `NA` values in all fields.
#' @author Alessandro Oggioni, PhD \email{alessandro.oggioni@@cnr.it}
#' @author Alice Lenzi, PhD \email{alice.lenzi@@crea.gov.it}
#' @author Alessandro Campanaro, PhD \email{alessandro.campanaro@@crea.gov.it}
#' @importFrom httr2 request req_method req_headers req_retry req_perform resp_status resp_body_json
#' @importFrom dplyr tibble select
#' @export
#' @examples
#' \dontrun{
#' ## Not run:
#' # with 1 IUCN red list conservation status declared
#' # Sclerophrys pantherina
#' get_conservation_status(
#'   taxon.id = 517449
#' )
#' 
#' # without IUCN red list conservation status declared
#' # Protoparmeliopsis muralis
#' get_conservation_status(
#'   taxon.id = 632126
#' )
#' 
#' # with 3 IUCN red list conservation status declared
#' # Falco tinnunculus
#' get_conservation_status(
#'   taxon.id = 472766
#' )
#'
#' }
#' ## End (Not run)
#'
### get_conservation_status
get_conservation_status <- function(taxon.id) {
  # helper: empty tibble with consistent structure
  empty_tbl <- dplyr::tibble(
      status = NA_character_,
      authority = NA_character_,
      scope_of_assesment = NA_character_,
      url = NA_character_
    )
  iNat_api_url <- paste0("https://api.inaturalist.org/v1/taxa/", taxon.id)
  
  response <- tryCatch({
    httr2::request(iNat_api_url) |> 
      httr2::req_method("GET") |> 
      httr2::req_headers(Accept = "application/json") |> 
      httr2::req_retry(max_tries = 3, max_seconds = 120) |> 
      httr2::req_perform()
  }, error = function(e) {
    return(NULL)
  })
  
  # check response
  if (is.null(response) || httr2::resp_status(response) != 200) {
    return(empty_tbl)
  }
  
  # parse JSON
  data <- tryCatch({
    httr2::resp_body_json(response, simplifyVector = TRUE)
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(data) || length(data$results) == 0) {
    return(empty_tbl)
  }
  
  # conservation statuses
  cons_status <- data$results$conservation_statuses[[1]]
  
  if (is.null(cons_status) ||
      length(cons_status) == 0 ||
      (is.data.frame(cons_status) && nrow(cons_status) == 0)
    ) {
    return(empty_tbl)
  }
  
  # filter only IUCN
  cons_status <- cons_status[cons_status$authority == "IUCN Red List", ]
  
  if (nrow(cons_status) == 0) {
    return(empty_tbl)
  }
  
  # clean output
  cons_status <- cons_status[cons_status$authority == "IUCN Red List", ]
  
  if (nrow(cons_status) == 0) {
    return(empty_tbl)
  }
  
  place_name <- vapply(seq_len(nrow(cons_status)), function(i) {
    p <- cons_status$place[i,]
    if (is.null(p)) return("Globally")
    if (is.data.frame(p) && "name" %in% names(p)) {
      name_val <- p$name[1]
      if (is.na(name_val)) return("Globally")
      return(as.character(name_val))
    }
    if (is.list(p) && !is.null(p[["name"]])) {
      name_val <- p[["name"]][1]
      if (is.na(name_val)) return("Globally")
      return(as.character(name_val))
    }
    "Globally"
  }, FUN.VALUE = character(1))
  
  return(tibble::tibble(
    status = as.character(cons_status$status),
    authority = as.character(cons_status$authority),
    scope_of_assesment = place_name,
    url = as.character(cons_status$url)
  ))
}

#' Get nativeness degree for a taxon from iNaturalist and EASIN
#' @description `r lifecycle::badge("stable")`
#' Queries the iNaturalist API to retrieve the establishment means
#' (nativeness status and authority) for a given taxon, optionally filtered
#' by country, as provided by iNaturalist checklists. Additionally queries
#' the EASIN (European Alien Species Information Network) database to retrieve
#' alien species information for the same taxon.
#'
#' Data are sourced from:
#' \enumerate{
#'   \item iNaturalist checklists —
#'     \url{https://forum.inaturalist.org/t/updating-iucn-red-list-conservation-statuses/25712}
#'   \item EASIN — European Alien Species Information Network —
#'     \url{https://easin.jrc.ec.europa.eu/easin}
#' }
#'
#' @param taxon.id \code{integer} or \code{character}. The iNaturalist taxon ID
#'   to query.
#' @param country \code{character}. The country name to filter results by
#'   (e.g., \code{"Italy"}). Must match the place name as recorded in
#'   iNaturalist. If \code{NULL}, a warning is issued and an empty result
#'   is returned to avoid ambiguous cross-country data.
#'
#' @return A \code{\link[dplyr]{tibble}} with one row and one list-column:
#'   \describe{
#'     \item{establishmentMeans}{\code{list} of one-row tibbles, each
#'       containing:
#'       \describe{
#'         \item{iNat_nativeness}{\code{character}. The establishment means
#'           value from iNaturalist (e.g., \code{"native"},
#'           \code{"introduced"}), or \code{NA} if not available.}
#'         \item{iNat_authority}{\code{character}. The checklist title
#'           associated with the establishment means in iNaturalist (e.g.,
#'           \code{"Italy Check List"}), or \code{NA} if not available.}
#'         \item{iNat_checkList_uri}{\code{character}. The URL of the
#'           iNaturalist checklist associated with the establishment means,
#'           or \code{NA} if not available.}
#'         \item{EASIN_url}{\code{character}. URL of the EASIN species
#'           factsheet, or \code{NA} if the taxon is not in EASIN.}
#'         \item{EASIN_id}{\code{character}. EASIN species identifier,
#'           or \code{NA} if not available.}
#'         \item{EASIN_LSID}{\code{character}. Life Science Identifier
#'           for the species in EASIN
#'           (e.g., \code{"urn:lsid:easin.jrc.ec.europa.eu:species:XXXX"}),
#'           or \code{NA} if not available.}
#'         \item{EASIN_firstIntroductionsInEU_year}{\code{character}. Year of
#'           first introduction in the EU as recorded in EASIN, or \code{NA}
#'           if not available.}
#'         \item{EASIN_firstIntroductions_Country}{\code{character}. Country
#'           of first introduction in the EU as recorded in EASIN, or
#'           \code{NA} if not available.}
#'         \item{EASIN_status}{\code{character}. Alien species status as
#'           recorded in EASIN, or \code{NA} if not available.}
#'         \item{EASIN_hasImpact}{\code{character}. Whether the species has
#'           a documented impact as recorded in EASIN (\code{"True"} or
#'           \code{"False"}), or \code{NA} if not available.}
#'         \item{EASIN_IsEUConcern}{\code{character}. Whether the species is
#'           listed as a species of EU concern under Regulation (EU)
#'           No 1143/2014 (\code{"True"} or \code{"False"}), or \code{NA}
#'           if not available.}
#'       }
#'     }
#'   }
#'
#' @note
#' The establishment means information is sourced from iNaturalist and may
#' refer to the IUCN Red List. It may not always be up to date.
#'
#' EASIN data are retrieved from the JRC API
#' (\url{https://easin.jrc.ec.europa.eu/apixg/catxg/term/}) and reflect
#' the information available in the EASIN catalogue. If the taxon is not
#' present in EASIN, all \code{EASIN_*} fields are \code{NA}.
#' 
#' \strong{Disclaimer}: EASIN reports alien (i.e., non-native) status at the
#' country level and does not distinguish cases where a species is non-native
#' only in specific sub-national areas (e.g., islands) but native in others
#' within the same country. Therefore, EASIN information may overgeneralize
#' the nativeness status when applied to heterogeneous territories such as
#' Italy
#'
#' @seealso
#' \code{\link{add_nativeness_to_occ}} for applying this function across
#' a full occurrence tibble.
#' \code{\link{.assign_EASIN_info}} for the underlying EASIN API call.
#'
#' @author Alessandro Oggioni, PhD \email{alessandro.oggioni@@cnr.it}
#' @author Alice Lenzi, PhD \email{alice.lenzi@@crea.gov.it}
#' @author Alessandro Campanaro, PhD \email{alessandro.campanaro@@crea.gov.it}
#'
#' @importFrom httr2 request req_method req_headers req_retry
#' @importFrom httr2 req_perform resp_status resp_body_json
#' @importFrom dplyr tibble
#' @importFrom purrr keep
#'
#' @examples
#' \dontrun{
#' # Get nativeness and EASIN info for a taxon in Italy
#' get_nativeness_degree(taxon.id = 48484, country = "Italy")
#'
#' # Species of EU concern example
#' get_nativeness_degree(taxon.id = 61976, country = "Italy")
#' }
#'
### get_nativeness_degree
get_nativeness_degree <- function(taxon.id, country = NULL) {
  
  # Helper: nested tibble returned in case of error or missing data
  empty_nested <- dplyr::tibble(
    establishmentMeans = list(
      dplyr::tibble(
        iNat_nativeness = NA_character_,
        iNat_authority = NA_character_,
        iNat_checkList_uri = NA_character_,
        EASIN_url = NA_character_,
        EASIN_id = NA_character_,
        EASIN_LSID = NA_character_,
        EASIN_firstIntroductionsInEU_year = NA_character_,
        EASIN_firstIntroductions_Country = NA_character_,
        EASIN_status = NA_character_,
        EASIN_hasImpact = NA_character_,
        EASIN_IsEUConcern = NA_character_,
      )
    )
  )
  
  # A country must be specified to avoid returning ambiguous cross-country data
  if (is.null(country)) {
    warning("No country specified: returning empty result to avoid ambiguous data.")
    return(empty_nested)
  }
  
  iNat_api_url <- paste0("https://api.inaturalist.org/v1/taxa/", taxon.id)
  
  # Perform the API request with retry logic; return NULL on failure
  response <- tryCatch({
    httr2::request(iNat_api_url) |>
      httr2::req_method("GET") |>
      httr2::req_headers(Accept = "application/json") |>
      httr2::req_retry(max_tries = 3, max_seconds = 120) |>
      httr2::req_perform()
  }, error = function(e) NULL)
  
  # Return empty tibble if request failed or returned a non-200 status
  if (is.null(response) || httr2::resp_status(response) != 200) {
    return(empty_nested)
  }
  
  # Parse JSON body; return empty tibble on parsing failure
  parsed <- tryCatch({
    httr2::resp_body_json(response)
  }, error = function(e) NULL)
  
  # parse info from EASIN EU Database
  specie_name <- parsed$results[[1]]$name
  easin_info <- .assign_EASIN_info(specie_name = specie_name)
  
  # Return empty tibble if parsing failed or no results found
  if (is.null(parsed) || length(parsed$results) == 0) {
    return(empty_nested)
  }
  
  listed_taxa <- parsed$results[[1]]$listed_taxa
  
  # Return empty tibble if no listed_taxa field is present
  if (is.null(listed_taxa)) {
    return(empty_nested)
  }
  
  # Filter listed_taxa to match only the specified country
  record <- purrr::keep(
    listed_taxa,
    ~ !is.null(.x$place$name) && .x$place$name == country
  )
  
  # Return empty tibble if no record matches the specified country
  if (length(record) == 0) {
    return(empty_nested)
  }
  
  # Take the first matching record for the specified country
  record <- record[[1]]
  
  nativeness_value <- record$establishment_means
  authority_value <- record$list$title
  checkList_uri <- paste0("https://www.inaturalist.org/lists/", record$list$id)
  
  # Return a tibble with establishmentMeans as a nested list-column
  dplyr::tibble(
    establishmentMeans = list(
      dplyr::tibble(
        iNat_nativeness = if (is.null(nativeness_value)) NA_character_ else as.character(nativeness_value),
        iNat_authority = if (is.null(authority_value))  NA_character_ else as.character(authority_value),
        iNat_checkList_uri = if (is.null(checkList_uri))  NA_character_ else as.character(checkList_uri),
        easin_info
      )
    )
  )
}

#' Get EUNIS Legal Information for a Species Using iNaturalist Taxon ID
#' @description `r lifecycle::badge("stable")`
#' This function takes a \code{taxon.id} from iNaturalist, retrieves the corresponding
#' scientific name from the iNaturalist API, searches the EUNIS database, and extracts
#' the legal information related to the EU Habitats Directive (92/43/EEC) and Birds Directive (2009/147/EC),
#' as delivered by data retrieved from the [EUNIS](https://eunis.eea.europa.eu/) species database.
#' 
#' @param taxon.id Integer. The taxon ID of the species in iNaturalist.
#'
#' @return A tibble with the following columns:
#'   \describe{
#'     \item{taxon.id}{iNaturalist taxon ID provided as input}
#'     \item{scientific_name}{Scientific name of the species retrieved from iNaturalist}
#'     \item{`Legal text`}{Legal directive text from EUNIS (92/43/EEC or 2009/147/EC)}
#'     \item{Annex}{Annex information from EUNIS table}
#'   }
#'
#' @author Alessandro Oggioni, PhD \email{alessandro.oggioni@@cnr.it}
#'
#' @importFrom httr2 request req_method req_headers req_retry req_perform
#' @importFrom httr2 resp_status resp_body_json
#' @importFrom rvest read_html html_elements html_attr html_table
#' @importFrom dplyr tibble filter select mutate
#' @importFrom purrr keep
#' @export
#'
#' @examples
#' \dontrun{
#' ## Not run:
#' get_eunis_legal_info(taxon.id = 61749)
#' }
#' ## End (Not run)
#'
### get_eunis_legal_info
get_eunis_legal_info <- function(taxon.id) {
  safe_return <- function(taxon.id, scientific_name = NA) {
    dplyr::tibble(
      taxon.id        = taxon.id,
      scientific_name = scientific_name,
      `Legal text`    = NA,
      Annex           = NA
    )
  }
  
  # --- 1. Extract scientific name from iNaturalist ---
  url_inat <- paste0("https://api.inaturalist.org/v1/taxa/", taxon.id)
  
  res <- tryCatch({
    httr2::request(url_inat) |>
      httr2::req_method("GET") |>
      httr2::req_headers(Accept = "application/json") |>
      httr2::req_retry(max_tries = 3, max_seconds = 120) |>
      httr2::req_perform()
  }, error = function(e) NULL)
  
  if (is.null(res) || httr2::resp_status(res) != 200) {
    warning("Error in iNaturalist API request")
    return(safe_return(taxon.id))
  }
  
  data <- tryCatch({
    httr2::resp_body_json(res, simplifyVector = TRUE)
  }, error = function(e) NULL)
  
  if (is.null(data) || length(data$results) == 0) {
    return(safe_return(taxon.id))
  }
  
  scientific_name <- data$results$name[1]
  
  # --- 2. Search species on EUNIS ---
  url_specie <- paste0(
    "https://eunis.eea.europa.eu/species-names-result.jsp?typeForm=0&showScientificName=true&searchVernacular=false&sort=1&ascendency=1&showGroup=false&showOrder=false&showFamily=false&showValidName=false&relationOp=2&scientificName=",
    URLencode(scientific_name)
  )
  
  link <- tryCatch({
    url_specie |>
      rvest::read_html() |>
      rvest::html_elements("a") |>
      rvest::html_attr("href") |>
      na.omit() |>
      as.vector() |>
      (\(x) x[grepl("^species/\\d+$", x)])()  # bug fix: pattern primo, x secondo
  }, error = function(e) character(0))
  
  if (length(link) == 0) {
    return(safe_return(taxon.id, scientific_name))
  }
  
  m <- regexpr("\\d+", link[1])
  species_id <- if (m != -1) regmatches(link[1], m) else NA_character_
  
  if (is.na(species_id)) {
    return(safe_return(taxon.id, scientific_name))
  }
  
  url_eunis <- paste0("https://eunis.eea.europa.eu/species/", species_id)
  
  # --- 3. Read EUNIS page and extract table ---
  page <- tryCatch({
    rvest::read_html(url_eunis) |>
      rvest::html_elements("table") |>
      rvest::html_table(fill = TRUE)
  }, error = function(e) NULL)
  
  if (is.null(page) || length(page) == 0) {
    return(safe_return(taxon.id, scientific_name))
  }
  
  # trova tabella con "Legal"
  df_legal_list <- purrr::keep(
    page,
    ~ any(grepl("Legal", names(.x)))  # bug fix: pattern primo, names(.x) secondo
  )
  
  if (length(df_legal_list) == 0) {
    return(safe_return(taxon.id, scientific_name))
  }
  
  df_legal <- df_legal_list[[1]]
  
  # verifica colonne attese
  if (!all(c("Legal text", "Annex") %in% names(df_legal))) {
    return(safe_return(taxon.id, scientific_name))
  }
  
  # filtra direttive EU
  df_legal <- df_legal |>
    dplyr::filter(grepl("92/43/EEC|2009/147/EC", `Legal text`)) |>  # bug fix: pattern primo
    dplyr::select(`Legal text`, Annex) |>
    dplyr::mutate(
      taxon.id        = taxon.id,
      scientific_name = scientific_name
    ) |>
    dplyr::select(taxon.id, scientific_name, `Legal text`, Annex)
  
  if (nrow(df_legal) == 0) {
    return(safe_return(taxon.id, scientific_name))
  }
  
  return(df_legal)
}

#' Enrich iNaturalist specific project observations with IUCN conservation status
#' @description `r lifecycle::badge("experimental")`
#' This function enriches all the iNaturalist project observations with the
#' IUCN Red List conservation status thanks to the `get_conservation_status()`
#' function.
#' The observations are filtered to include only 'Research grade' data also
#' that meet the following a valid date, an 'open' geographic location, the
#' presence of a photo or sound, and the exclusion of captive or cultivated
#' organisms.
#' @param project_name A `string` value representing the name of the
#' iNaturalist project.
#' @return A `data.frame` object representing the iNaturalist project
#' observations, as well as the conservation status of the taxa.
#' The structure of the `data.frame` follows the original structure as obtained
#' by the `rinat::get_inat_obs_project()` function where the type arguments is
#' set to "observations".
#' The content of the `data.frame` returned is enriched with the following
#' columns:
#' - `info_title_proj`: A `character` value representing the title of the
#' project, as obtained by the `rinat::get_inat_obs_project()` function where
#' the type arguments is set to "info";
#' - `info_slug_proj`: A `character` value representing the slug of the
#' project;
#' - `info_taxa_num_proj`: A `numeric` value representing the number of taxa
#' in the project;
#' - `info_place_uuid_proj`: A `character` value representing the place uuid
#' of the project within iNaturalist system;
#' - `status`: A `character` value representing the conservation status of the
#' taxa by parsing the Taxa iNaturalist API JSON response, based on the
#' `taxon.id` value and only if the authority is the 'IUCN Red List'. In the
#' case the JSON parsing fails, it will returns an Error message and fill this
#' column with NA.
#' The function also prints some console messages to inform the user about the
#' status results for each `taxon.id`, as well as the number of taxa that
#' encountered a JSON parsing error.
#' @author Alessandro Oggioni, PhD \email{alessandro.oggioni@@cnr.it}
#' @author Alice Lenzi, PhD \email{alice.lenzi@@crea.gov.it}
#' @author Alessandro Campanaro, PhD \email{alessandro.campanaro@@crea.gov.it}
#' @importFrom rinat get_inat_obs_project
#' @importFrom dplyr mutate filter tibble select distinct left_join
#' @importFrom purrr map_dfr walk2
#' @importFrom stats na.omit
#' @export
#' @examples
#' \dontrun{
#' ## Not run:
#' add_iucn_to_obs(project_name = "LTER site Montagna di Torricchio")
#' }
#' ## End (Not run)
#'
### add_iucn_to_obs
add_iucn_to_obs <- function(project_name) {
  proj_alias <- gsub(" ", "-", tolower(project_name))
  
  deps <- c("rinat")
  deps_missing <- !sapply(deps, requireNamespace, quietly = TRUE)
  
  if (sum(deps_missing) > 0) {
    stop(
      "You need to install the following Suggested packages to use this function.\n",
      "Please install them with:\n",
      "install.packages(c(\"rinat\"))"
    )
  }
  # Download iNaturalist project info by rinat package
  iNat_project_info <- rinat::get_inat_obs_project(
    proj_alias,
    type = "info",
    raw = TRUE
  )
  
  # Download iNaturalist project observations by rinat package
  obs_iNat_project <- rinat::get_inat_obs_project(
    proj_alias,
    type = "observations",
    raw = TRUE
  ) |>
    dplyr::mutate(
      info_title_proj = iNat_project_info$title,
      info_slug_proj = iNat_project_info$slug,
      info_taxa_num_proj = iNat_project_info$taxa_count,
      info_place_uuid_proj = iNat_project_info$raw$rule_place$uuid
    )
  
  # Filter records to include only research-grade data
  obs_iNat_project_research <- obs_iNat_project |>
    dplyr::filter(quality_grade == "research")
  
  # Extract unique taxon.ids
  unique_taxon.ids <- unique(obs_iNat_project_research$taxon.id)
  
  # Progressive counter
  record_counter <- 0
  
  # Process only unique taxon.ids
  status_results <- purrr::map_dfr(unique_taxon.ids, function(taxon.id) {
    record_counter <<- record_counter + 1
    status <- get_conservation_status(taxon.id)
    
    # Print progress
    message("✅ Progressive: ", record_counter, " | iNat taxon id: ", taxon.id, " | Status: ", status)
    
    dplyr::tibble(taxon.id = taxon.id, status = status)
  })
  
  # Filter taxon.id with JSON parsing errors
  taxon_errors <- status_results |> 
    dplyr::filter(is.na(status)) |>  # Errors are stored as NA
    dplyr::inner_join(obs_iNat_project_research_valid, by = "taxon.id") |> 
    dplyr::select(taxon.id, taxon.name, taxon.common_name.name) |>
    dplyr::distinct()
  
  # Print messages in the console
  if (nrow(taxon_errors) > 0) {
    message("WARNING: ", nrow(taxon_errors), " taxa encountered a JSON parsing error.")
    
    # Print each taxon.id with scientific and common name
    purrr::walk2(
      taxon_errors$taxon.id, 
      taxon_errors$taxon.name,
      ~ message("❌ Error for Taxon ID: ", .x, " | Scientific name: ", .y)
    )
  } else {
    message("✅ No JSON parsing errors found for taxa.")
  }
  
  # Merge the obtained results with the original dataframe
  obs_iNat_project_research_valid <- obs_iNat_project_research_valid |> 
    dplyr::left_join(status_results, by = "taxon.id")
  
  return(obs_iNat_project_research_valid)
}

#' Enrich eLTER site iNaturalist occurrences with IUCN conservation status
#' @description `r lifecycle::badge("stable")`
#' This function enriches all the eLTER site iNaturalist occurrences acquired
#' by the \code{ReLTER::get_site_speciesOccurrences()} function with the IUCN
#' Red List conservation status as recorded in iNaturalist.
#'
#' Observations are filtered to include only "Research Grade" data that meet
#' the following criteria: have a valid date and exclude captive or cultivated
#' organisms.
#'
#' Observations are not filtered with respect to geoprivacy. In iNaturalist,
#' each observation can be assigned one of three geoprivacy levels: open,
#' obscured, or private (for more information, see:
#' \url{https://www.inaturalist.org/pages/geoprivacy}). Given the need to
#' gather information on species with critical conservation status, even if
#' the geographic information is not precise, it is still important to record
#' the presence of a species within a given area.
#'
#' The function queries the iNaturalist Taxa API once per unique \code{taxon.id}
#' and stores the returned IUCN assessments (if available) in a nested
#' list-column.
#'
#' If not already present, the function automatically assigns eLTER Standard
#' Observations to each record via \code{\link{.assign_eLTER_SOs}}, adding
#' three logical columns: \code{SOBIO_014} (Flying insects — Insecta),
#' \code{SOBIO_017} (Plants), and \code{SOBIO_018} (Acoustic recording — Aves, 
#' Anura, Chiroptera, Orthoptera). Orthoptera contribute to SOBIO_014 and 
#' SOBIO_018 simultaneously.
#' If the columns are already present (e.g. because a previous enrichment function
#' was already run), the assignment step is skipped.
#'
#' @param occ_eLTER A \code{data.frame} or \code{sf} object representing the
#'   occurrences of an eLTER site acquired by the
#'   \code{ReLTER::get_site_speciesOccurrences()} function (typically
#'   \code{occ_eLTER$inat}).
#'
#' @return A \code{data.frame} or \code{sf} object with the same structure as
#'   the input, enriched with the following additional columns:
#'   \describe{
#'     \item{status_IUCN}{A list-column (\code{list} of \code{tibble}s)
#'       containing the IUCN Red List assessments retrieved from iNaturalist
#'       for each \code{taxon.id}. Each tibble has the columns \code{status},
#'       \code{authority}, \code{name}, and \code{url}. If no IUCN information
#'       is available, a tibble with \code{NA} values is stored.}
#'     \item{has_IUCN}{A \code{logical} value indicating whether at least one
#'       valid (non-\code{NA}) IUCN status is available for the given
#'       \code{taxon.id}.}
#'     \item{SOBIO_014}{\code{logical}. Whether the observation contributes to
#'       Flying insects (SOBIO_014) eLTER Standard Observation.
#'       Assigned only if not already present.}
#'     \item{SOBIO_017}{\code{logical}. Whether the observation contributes to
#'       Vegetation composition (SOBIO_017) eLTER Standard Observation.
#'       Assigned only if not already present.}
#'     \item{SOBIO_018}{\code{logical}. Whether the observation contributes to
#'       Acoustic recording (SOBIO_018) eLTER Standard Observation.
#'       Assigned only if not already present.}
#'   }
#'
#' @note
#' The IUCN conservation status is retrieved from the iNaturalist Taxa API and
#' reflects the information available within iNaturalist. This may not be up
#' to date with respect to the official IUCN Red List.
#'
#' eLTER Standard Observation assignments are based on taxonomic ancestry
#' (\code{taxon.ancestor_ids}) retrieved from the iNaturalist API.
#'
#' @seealso
#' \code{\link{get_conservation_status}},
#' \code{\link{add_nativeness_to_occ}},
#' \code{\link{add_eunis_legal_to_occ}},
#' \code{\link{obs_SO_pie_chart}}
#' 
#' @details
#' The IUCN conservation status is retrieved from the iNaturalist Taxa API and
#' reflects the information available within iNaturalist. This information may
#' not be up to date with respect to the official IUCN Red List.
#' @author Alessandro Oggioni, PhD \email{alessandro.oggioni@@cnr.it}
#' @author Alice Lenzi, PhD \email{alice.lenzi@@crea.gov.it}
#' @author Alessandro Campanaro, PhD \email{alessandro.campanaro@@crea.gov.it}
#' @importFrom dplyr filter select distinct left_join mutate tibble
#' @importFrom purrr map_dfr map_lgl walk walk2
#' @export
#' @examples
#' \dontrun{
#' ## Not run:
#' # Download taxa occurrences from iNaturalist using ReLTER's
#' # get_site_speciesOccurrences() function
#' # e.g. Montagna di Torricchio eLTER site
#' deimsid <- "https://deims.org/6b62feb2-61bf-47e1-b97f-0e909c408db8"
#' 
#' occ_eLTER <- ReLTER::get_site_speciesOccurrences(
#'   deimsid = deimsid,
#'   list_DS = "inat",
#'   show_map = FALSE,
#'   limit = 50
#' )
#' 
#' occ <- add_iucn_to_occ(
#'   occ_eLTER = occ_eLTER$inat
#' )
#' }
#' ## End (Not run)
#' 
### add_iucn_to_occ
add_iucn_to_occ <- function(occ_eLTER) {
  # assign eLTER SOs if not already present
  if (!"SOBIO_014" %in% names(occ_eLTER)) {
    message("ℹ️ Assigning eLTER Standard Observations (SOBIO_014, SOBIO_017, and SOBIO_018)...\n----\n")
    occ_eLTER <- .assign_eLTER_SOs(occ_eLTER)
  } else {
    message("ℹ️ eLTER Standard Observations already assigned — skipping.\n----\n")
  }
  
  occ_in_site_research_valid <- occ_eLTER |>
    dplyr::filter(
      quality_grade == "research",
      !is.na(observed_on),
      captive == FALSE
    )
  # Standard empty tibble (no NULL anywhere)
  empty_status_tbl <- dplyr::tibble(
    status = NA_character_,
    authority = NA_character_,
    name = NA_character_,
    url = NA_character_
  )
  # Helper: check if status is valid
  has_valid_status <- function(x) {
    "status" %in% names(x) && any(!is.na(x$status))
  }
  unique_taxon.ids <- unique(occ_in_site_research_valid$taxon.id)
  record_counter <- 0
  status_results <- purrr::map_dfr(unique_taxon.ids, function(taxon.id) {
    record_counter <<- record_counter + 1
    status_tbl <- tryCatch(
      get_conservation_status(taxon.id),
      error = function(e) NULL
    )
    # Normalize output
    # if (is.na(status)) {
    #   status_tbl <- empty_status_tbl
    # } else if (inherits(status, "data.frame")) {
    #   if (nrow(status) == 0) {
    #     status_tbl <- empty_status_tbl
    #   } else {
    #     status_tbl <- status
    #   }
    # } else {
    #   status_tbl <- empty_status_tbl
    # }
    # Check validity
    valid <- has_valid_status(status_tbl)
    # Logging
    if (!valid) {
      message("⚠️ Progressive taxon record: ", record_counter,
              " | iNat taxon id: ", taxon.id,
              " | No IUCN status recorded in iNaturalist")
    } else {
      message("✅ Progressive taxon record:  ", record_counter,
              " | iNat taxon id: ", taxon.id,
              " | Records found: ", sum(!is.na(status_tbl$status)),
              " | Authority: IUCN Red List")
    }
    
    dplyr::tibble(
      taxon.id = taxon.id,
      status_IUCN = list(status_tbl)
    )
  })
  # Join names (ensure one row per taxon.id)
  taxa_info <- occ_in_site_research_valid |>
    dplyr::select(taxon.id, name, taxon.preferred_common_name) |>
    dplyr::distinct(taxon.id, .keep_all = TRUE)
  status_results <- status_results |>
    dplyr::left_join(taxa_info, by = "taxon.id") |>
    dplyr::mutate(
      has_IUCN = purrr::map_lgl(status_IUCN, has_valid_status)
    )
  # Split
  taxon_no_errors <- dplyr::filter(status_results, has_IUCN)
  taxon_errors   <- dplyr::filter(status_results, !has_IUCN)
  # Summary log
  message(
    "\n----\nThe conservation status here, if present, is the one reported in iNaturalist, referring to the IUCN Red List. It may not be up to date.\n----\n",
    "⚠️ ", nrow(taxon_errors),
    " taxa encountered no IUCN Red List status information in iNaturalist.\n"
  )
  purrr::walk2(
    taxon_errors$taxon.id,
    taxon_errors$name,
    ~ message("⚠️ The Taxon ID: ", .x,
              " | Scientific name: ", .y,
              " | it does not have an IUCN status recorded in iNaturalist.")
  )
  message(
    "\n✅ ", nrow(taxon_no_errors),
    " taxa successfully parsed and ready for use. Please, remember that this information may not be up to date.\n"
  )
  purrr::walk(
    seq_len(nrow(taxon_no_errors)),
    function(i) {
      n_records <- sum(!is.na(taxon_no_errors$status_IUCN[[i]]$status))
      message(
        "✅ The Taxon ID: ", taxon_no_errors$taxon.id[i],
        " | Scientific name: ", taxon_no_errors$name[i],
        " | it does have an IUCN status recorded in iNaturalist (",
        n_records, " records)."
      )
    }
  )
  # Final join
  occ_in_site_research_valid <- occ_in_site_research_valid |>
    dplyr::left_join(
      status_results |>
        dplyr::select(taxon.id, has_IUCN, status_IUCN),
      by = "taxon.id"
    )
  return(occ_in_site_research_valid)
}

#' Add nativeness information to iNaturalist occurrence records
#' @description `r lifecycle::badge("stable")`
#' Filters a tibble of iNaturalist occurrence records to retain only
#' research-grade, non-captive observations with a valid date, then fetches
#' establishment means information from iNaturalist for each unique taxon
#' via \code{\link{get_nativeness_degree}}. The results are joined back to
#' the filtered occurrence tibble as a nested \code{establishmentMeans}
#' list-column.
#'
#' If not already present, the function automatically assigns eLTER Standard
#' Observations to each record via \code{\link{.assign_eLTER_SOs}}, adding
#' three logical columns: \code{SOBIO_014} (Flying insects — Insecta),
#' \code{SOBIO_017} (Plants), and \code{SOBIO_018} (Acoustic recording — Aves, 
#' Anura, Chiroptera, Orthoptera). Orthoptera contribute to SOBIO_014 and 
#' SOBIO_018 simultaneously.
#' If the columns are already present (e.g. because a previous enrichment function
#' was already run), the assignment step is skipped.
#'
#' @param occ_eLTER \code{\link[dplyr]{tibble}}. A tibble of iNaturalist
#'   occurrence records, typically obtained via
#'   \code{ReLTER::get_site_speciesOccurrences()}. Must contain at least the
#'   columns \code{quality_grade}, \code{observed_on}, \code{captive},
#'   \code{taxon.id}, and \code{taxon.ancestor_ids}.
#' @param country \code{character}. The country name to filter establishment
#'   means by (e.g., \code{"Italy"}). Must match the place name as recorded
#'   in iNaturalist. Cannot be \code{NULL}.
#'
#' @return A \code{\link[dplyr]{tibble}} of filtered occurrence records
#'   (research-grade, non-captive, with valid date) with the following
#'   additional columns:
#'   \describe{
#'     \item{has_establishmentMeans}{\code{logical}. Whether at least one
#'       valid (non-\code{NA}) establishment means value is available for
#'       the given \code{taxon.id}.}
#'     \item{establishmentMeans}{list-column. Each element is a one-row tibble
#'       containing \code{iNat_nativeness}, \code{iNat_authority} (from
#'       iNaturalist), plus the EASIN fields \code{EASIN_url},
#'       \code{EASIN_id}, \code{EASIN_LSID},
#'       \code{EASIN_firstIntroductionsInEU_year},
#'       \code{EASIN_firstIntroductions_Country}, \code{EASIN_status},
#'       \code{EASIN_hasImpact}, \code{EASIN_IsEUConcern}. All fields
#'       are \code{NA} if not available.}
#'     \item{SOBIO_014}{\code{logical}. Whether the observation contributes to
#'       Flying insects (SOBIO_014) eLTER Standard Observation.
#'       Assigned only if not already present.}
#'     \item{SOBIO_017}{\code{logical}. Whether the observation contributes to
#'       Vegetation composition (SOBIO_017) eLTER Standard Observation.
#'       Assigned only if not already present.}
#'     \item{SOBIO_018}{\code{logical}. Whether the observation contributes to
#'       Acoustic recording (SOBIO_018) eLTER Standard Observation.
#'       Assigned only if not already present.}
#'   }
#'
#' @note
#' The establishment means information is sourced from iNaturalist and may
#' refer to the IUCN Red List. It may not always be up to date.
#'
#' eLTER Standard Observation assignments are based on taxonomic ancestry
#' (\code{taxon.ancestor_ids}) retrieved from the iNaturalist API.
#'
#' @seealso
#' \code{\link{get_nativeness_degree}},
#' \code{\link{add_iucn_to_occ}},
#' \code{\link{add_eunis_legal_to_occ}},
#' \code{\link{obs_SO_pie_chart}}
#' 
#' @note Progress messages are printed to the console for each taxon
#'   processed, including the iNaturalist taxon ID, nativeness status,
#'   and authority. A summary of taxa with and without establishment means
#'   is printed at the end.
#'
#'   The establishment means information is sourced from iNaturalist and
#'   may refer to the IUCN Red List. It may not always be up to date.
#'   
#' @author Alessandro Oggioni, PhD \email{alessandro.oggioni@@cnr.it}
#' @author Alice Lenzi, PhD \email{alice.lenzi@@crea.gov.it}
#' @author Alessandro Campanaro, PhD \email{alessandro.campanaro@@crea.gov.it}
#' @importFrom dplyr filter select distinct inner_join left_join tibble
#' @importFrom purrr map_dfr pluck walk2 map_chr
#' @export
#' @examples
#' \dontrun{
#' # Download taxa occurrences from iNaturalist using ReLTER's
#' # get_site_speciesOccurrences() function
#' # e.g. Montagna di Torricchio eLTER site
#' deimsid <- "https://deims.org/6b62feb2-61bf-47e1-b97f-0e909c408db8"
#'
#' occ_eLTER <- ReLTER::get_site_speciesOccurrences(
#'   deimsid = deimsid,
#'   list_DS = "inat",
#'   show_map = FALSE,
#'   limit = 50
#' )
#'
#' site_boundary <- ReLTER::get_site_info(deimsid = deimsid)
#'
#' occ <- add_nativeness_to_occ(
#'   occ_eLTER = occ_eLTER$inat,
#'   country = site_boundary$country
#' )
#' }
#'
### add_nativeness_to_occ
add_nativeness_to_occ <- function(occ_eLTER, country) {
  if (is.null(country)) {
    stop("Argument `country` is required. Please specify a country name (e.g., 'Italy').")
  }
  
  # assign eLTER SOs if not already present
  if (!"SOBIO_014" %in% names(occ_eLTER)) {
    message("ℹ️ Assigning eLTER Standard Observations (SOBIO_014, SOBIO_017, and SOBIO_018)...\n----\n")
    occ_eLTER <- .assign_eLTER_SOs(occ_eLTER)
  } else {
    message("ℹ️ eLTER Standard Observations already assigned — skipping.\n----\n")
  }
  
  # Keep only research-grade, non-captive observations with a valid date
  occ_in_site_research_valid <- occ_eLTER |>
    dplyr::filter(quality_grade == "research") |>
    dplyr::filter(
      !is.na(observed_on) &
        captive == FALSE
    )
  
  # Extract unique taxon IDs to avoid redundant API calls
  unique_taxon.ids <- unique(occ_in_site_research_valid$taxon.id)
  
  # Progressive counter for console logging
  record_counter <- 0
  
  # Fetch establishment means for each unique taxon ID, filtered by country
  nativeness_results <- purrr::map_dfr(unique_taxon.ids, function(taxon.id) {
    record_counter <<- record_counter + 1
    
    raw <- get_nativeness_degree(taxon.id, country = country)
    
    # Extract nativeness and authority from the nested tibble for logging
    nativeness_val <- purrr::pluck(raw, "establishmentMeans", 1, "iNat_nativeness", 1)
    authority_val <- purrr::pluck(raw, "establishmentMeans", 1, "iNat_authority",  1)
    
    # Log result to console
    if (is.na(nativeness_val)) {
      message("⚠️ Progressive: ", record_counter,
              " | iNat taxon id: ", taxon.id,
              " | No Establishment Means recorded in iNaturalist for country: ", country)
    } else {
      message("✅ Progressive: ", record_counter,
              " | iNat taxon id: ", taxon.id,
              " | Status: ", nativeness_val,
              " | Authority: ", authority_val,
              " | Country: ", country)
    }
    
    # Return a one-row tibble with taxon.id and the nested establishmentMeans
    dplyr::tibble(
      taxon.id = taxon.id,
      establishmentMeans = raw$establishmentMeans
    )
  })
  
  # Helper to extract nativeness string from the list-column for filtering
  get_nat <- function(em_list) purrr::map_chr(em_list, ~ {
    val <- .x$iNat_nativeness %||% NA_character_
    if (length(val) == 0) return(NA_character_)
    if (length(val) > 1) return(val[1])
    val
  })
  
  nativeness_vec <- get_nat(nativeness_results$establishmentMeans)
  
  # Taxa with a valid establishment means value
  taxon_no_errors <- nativeness_results |>
    dplyr::filter(!is.na(nativeness_vec)) |>
    dplyr::inner_join(occ_in_site_research_valid, by = "taxon.id") |>
    dplyr::select(taxon.id, name, taxon.preferred_common_name) |>
    dplyr::distinct()
  
  # Taxa with missing establishment means (stored as NA)
  taxon_errors <- nativeness_results |>
    dplyr::filter(is.na(nativeness_vec)) |>
    dplyr::inner_join(occ_in_site_research_valid, by = "taxon.id") |>
    dplyr::select(taxon.id, name, taxon.preferred_common_name) |>
    dplyr::distinct()
  
  # Summary message about data provenance and country scope
  message(
    "\n----\nThe Establishment Means here, if present, is the one reported in iNaturalist ",
    "for country: ", country, ", referring to the IUCN Red List. It may not be up to date.\n----\n",
    "⚠️ ", nrow(taxon_errors), " taxa encountered no Establishment Means information.\n"
  )
  
  # Log each taxon without establishment means
  purrr::walk2(
    taxon_errors$taxon.id, taxon_errors$name,
    ~ message("⚠️ Taxon ID: ", .x, " | Scientific name: ", .y,
              " | No Establishment Means in iNaturalist for country: ", country)
  )
  
  message("\n✅ ", nrow(taxon_no_errors),
          " taxa successfully parsed. Remember: this info may not be up to date.\n")
  
  # Log each taxon with establishment means
  purrr::walk2(
    taxon_no_errors$taxon.id, taxon_no_errors$name,
    ~ message("✅ Taxon ID: ", .x, " | Scientific name: ", .y,
              " | Establishment Means available for country: ", country)
  )
  
  # Left join to attach the establishmentMeans list-column to the full occurrence tibble
  occ_in_site_research_valid |>
    dplyr::left_join(nativeness_results, by = "taxon.id") |>
    # has_establishmentMeans
    dplyr::mutate(
      has_establishmentMeans = !is.na(
        vapply(establishmentMeans, function(em) {
          val <- tryCatch(em$iNat_nativeness[[1]], error = function(e) NA_character_)
          if (is.null(val) || length(val) == 0) NA_character_ else as.character(val)
        }, FUN.VALUE = character(1))
      )
    )
}

#' Enrich iNaturalist occurrences with EUNIS legal framework information
#' @description `r lifecycle::badge("stable")`
#' This function enriches iNaturalist occurrences with legal protection
#' information extracted from the EUNIS database. It uses the scientific name
#' retrieved from the iNaturalist \code{taxon.id} to query EUNIS and extract
#' legal directives related to the EU Habitats Directive (92/43/EEC) and
#' Birds Directive (2009/147/EC).
#'
#' Observations are not filtered by geoprivacy or research grade. If a taxon
#' has no legal information in EUNIS, \code{NA} values are returned for
#' \code{directive} and \code{annex}.
#'
#' If not already present, the function automatically assigns eLTER Standard
#' Observations to each record via \code{\link{.assign_eLTER_SOs}}, adding
#' three logical columns: \code{SOBIO_014} (Flying insects — Insecta),
#' \code{SOBIO_017} (Plants), and \code{SOBIO_018} (Acoustic recording — Aves, 
#' Anura, Chiroptera, Orthoptera). Orthoptera contribute to SOBIO_014 and 
#' SOBIO_018 simultaneously.
#' If the columns are already present (e.g. because a previous enrichment function
#' was already run), the assignment step is skipped.
#'
#' @param occ_eLTER A \code{tibble} containing iNaturalist occurrences. Must
#'   contain the columns \code{taxon.id} and \code{taxon.ancestor_ids}.
#'
#' @return A \code{tibble} containing all original columns of \code{occ_eLTER}
#'   plus:
#'   \describe{
#'     \item{directive}{\code{character}. Legal directive text from EUNIS
#'       (92/43/EEC or 2009/147/EC), or \code{NA} if not found.}
#'     \item{annex}{\code{character}. Annex information from EUNIS table,
#'       or \code{NA} if not found.}
#'     \item{SOBIO_014}{\code{logical}. Whether the observation contributes to
#'       Flying insects (SOBIO_014) eLTER Standard Observation.
#'       Assigned only if not already present.}
#'     \item{SOBIO_017}{\code{logical}. Whether the observation contributes to
#'       Vegetation composition (SOBIO_017) eLTER Standard Observation.
#'       Assigned only if not already present.}
#'     \item{SOBIO_018}{\code{logical}. Whether the observation contributes to
#'       Acoustic recording (SOBIO_018) eLTER Standard Observation.
#'       Assigned only if not already present.}
#'   }
#'
#' @note
#' eLTER Standard Observation assignments are based on taxonomic ancestry
#' (\code{taxon.ancestor_ids}) retrieved from the iNaturalist API.
#'
#' @seealso
#' \code{\link{get_eunis_legal_info}},
#' \code{\link{add_iucn_to_occ}},
#' \code{\link{add_nativeness_to_occ}},
#' \code{\link{obs_SO_pie_chart}}
#' 
#' @author Alessandro Oggioni, PhD \email{alessandro.oggioni@@cnr.it}
#' @author Alice Lenzi, PhD \email{alice.lenzi@@crea.gov.it}
#' @author Alessandro Campanaro, PhD \email{alessandro.campanaro@@crea.gov.it}
#' @importFrom dplyr tibble mutate left_join rename
#' @importFrom purrr map_dfr
#' @export
#' @examples
#' \dontrun{
#' # Download taxa occurrences from iNaturalist using ReLTER's
#' # get_site_speciesOccurrences() function
#' # e.g. Montagna di Torricchio eLTER site
#' deimsid <- "https://deims.org/6b62feb2-61bf-47e1-b97f-0e909c408db8"
#' occ_iNat <- ReLTER::get_site_speciesOccurrences(
#'   deimsid = deimsid,
#'   list_DS = "inat",
#'   show_map = FALSE,
#'   limit = 50
#' )
#'
#' occ_legal <- add_eunis_legal_to_occ(
#'   occ_eLTER = occ_iNat$inat
#' )
#' }
#'
### add_eunis_legal_to_occ
add_eunis_legal_to_occ <- function(occ_eLTER) {
  
  # Check that taxon.id column exists
  if(!"taxon.id" %in% names(occ_eLTER)) {
    stop("The input tibble must contain a column named 'taxon.id'")
  }
  
  # assign eLTER SOs if not already present
  if (!"SOBIO_014" %in% names(occ_eLTER)) {
    message("ℹ️ Assigning eLTER Standard Observations (SOBIO_014, SOBIO_017, and SOBIO_018)...\n----\n")
    occ_eLTER <- .assign_eLTER_SOs(occ_eLTER)
  } else {
    message("ℹ️ eLTER Standard Observations already assigned — skipping.\n----\n")
  }
  
  # Extract unique taxon.ids
  unique_taxon.ids <- unique(occ_eLTER$taxon.id)
  
  record_counter <- 0
  
  # Map over unique taxon.ids to get legal info from EUNIS
  legal_results <- purrr::map_dfr(unique_taxon.ids, function(taxon.id) {
    
    record_counter <<- record_counter + 1
    
    # Retrieve legal info for this taxon.id
    legal <- tryCatch(
      get_eunis_legal_info(taxon.id),
      error = function(e) {
        warning("Error retrieving legal info for taxon.id: ", taxon.id)
        return(dplyr::tibble(
          taxon.id = taxon.id,
          scientific_name = NA,
          `Legal text` = NA,
          Annex = NA
        ))
      }
    )
    
    # Print progress
    if(all(is.na(legal$`Legal text`))) {
      message("⚠️ Progressive ", record_counter, 
              " | iNat taxon.id: ", taxon.id,
              " | No legal info in EUNIS")
    } else {
      message("✅ Progressive ", record_counter,
              " | iNat taxon.id: ", taxon.id,
              " | Legal info retrieved from EUNIS")
    }
    
    return(legal)
  })
  
  # Merge legal info with original occurrences
  occ_enriched <- occ_eLTER |>
    dplyr::left_join(
      legal_results |> dplyr::select(taxon.id, `Legal text`, Annex),
      by = "taxon.id"
    ) |>
    dplyr::rename(directive = `Legal text`, annex = Annex)
  
  return(occ_enriched)
}

#' Create Leaflet map for enriched iNaturalist occurrences
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Takes a tibble of iNaturalist occurrences already enriched with species
#' nativeness, IUCN conservation status (\code{\link{add_iucn_to_occ()}}),
#' nativeness (\code{\link{add_nativeness_to_occ()}}), and EUNIS legal 
#' information (\code{\link{add_eunis_to_occ()}}), and eLTER Standard
#' Observations (SOs) assignments. Verifies that all required columns are
#' present, builds observation-level HTML popups with linked references to
#' iNaturalist, IUCN Red List, and EUR-Lex directive pages, and returns an
#' interactive Leaflet map with colour-coded markers by iconic taxon group
#' and marker clustering.
#'
#' @param occ_enriched An \code{sf} tibble of iNaturalist occurrences enriched
#'   with the following columns:
#'   \describe{
#'     \item{establishmentMeans}{list-column of 1 × 11 tibbles fron iNaturalist
#'       \code{iNat_nativeness}, \code{iNat_authority}, and \code{iNat_checkList_uri},
#'       from EASIN \code{EASIN_url}, \code{EASIN_id}, \code{EASIN_LSID},
#'       \code{EASIN_firstIntroductionsInEU_year}, \code{EASIN_firstIntroductions_Country},
#'       \code{EASIN_status}, \code{EASIN_hasImpact}, \code{EASIN_IsEUConcern}
#'       produced by \code{\link{add_nativeness_to_occ}}.}
#'     \item{status_IUCN}{list-column of N × 4 tibbles with \code{status},
#'       \code{authority}, \code{scope_of_assesment} (geographic scope), and \code{url},
#'       produced by \code{\link{add_iucn_to_occ}}.}
#'     \item{directive}{character. EU directive name, produced by
#'       \code{\link{add_eunis_legal_to_occ}}.}
#'     \item{annex}{character. EU directive annex, produced by
#'       \code{\link{add_eunis_legal_to_occ}}.}
#'     \item{SOBIO_014}{logical. Whether the observation contributes to the
#'       eLTER Standard Observation Flying insects (SOBIO_014), assigned by
#'       the enrichment pipeline via \code{\link{.assign_eLTER_SOs}}.}
#'     \item{SOBIO_017}{logical. Whether the observation contributes to the
#'       eLTER Standard Observation Vegetation composition (SOBIO_017),
#'       assigned by the enrichment pipeline via \code{\link{.assign_eLTER_SOs}}.}
#'     \item{SOBIO_018}{logical. Whether the observation contributes to the
#'       eLTER Standard Observation Acoustic recording (SOBIO_018),
#'       assigned by the enrichment pipeline via \code{\link{.assign_eLTER_SOs}}.}
#'     \item{taxon.id}{integer. iNaturalist taxon ID.}
#'     \item{name}{character. Scientific name.}
#'     \item{taxon.preferred_common_name}{character. Common name.}
#'     \item{taxon.iconic_taxon_name}{character. Iconic taxon group
#'       (e.g. \code{"Aves"}, \code{"Plantae"}) used for marker colouring.}
#'     \item{taxon_geoprivacy}{character. Geoprivacy status of the taxon
#'       (\code{"open"}, \code{"obscured"}, \code{"private"}).}
#'     \item{quality_grade}{character. iNaturalist data quality grade.}
#'     \item{observed_on}{Date. Observation date.}
#'     \item{public_positional_accuracy}{numeric. Positional accuracy in
#'       metres.}
#'     \item{user.login}{character. iNaturalist username of the observer.}
#'     \item{uri}{character. URL of the iNaturalist observation record.}
#'     \item{taxon.default_photo.square_url}{character. URL of the taxon
#'       thumbnail photo shown in the popup.}
#'   }
#' @param site_boundary An \code{sf} object representing the eLTER site
#'   boundary polygon, as returned by \code{ReLTER::get_site_info()}.
#'   If \code{NULL} (default), no boundary polygon is drawn on the map.
#'
#' @return A \code{\link[leaflet]{leaflet}} map object with:
#'   \itemize{
#'     \item An OpenStreetMap base tile layer.
#'     \item An optional eLTER site boundary polygon layer.
#'     \item Circle markers for \code{"open"} observations (radius 6, full
#'       opacity), \code{"obscured"} observations (radius 10, reduced
#'       opacity), and \code{"unknown"} geoprivacy observations (radius 8,
#'       low opacity), all with marker clustering enabled.
#'     \item Popups per observation showing:
#'       \itemize{
#'         \item taxon photo, scientific name (linked to iNaturalist),
#'           common name, observation date, observer (linked to iNaturalist),
#'           and link to the observation record;
#'         \item data quality grade, geoprivacy level, and positional
#'           accuracy;
#'         \item IUCN Red List status per geographic scope, with links to
#'           the Red List assessments;
#'         \item establishment means and authority, with link to iNaturalist
#'           help;
#'         \item EU directive coverage (Habitats and Birds Directives) with
#'           links to EUR-Lex;
#'         \item eLTER Standard Observations the record contributes to
#'           (\code{SOBIO_014} Flying insects and/or \code{SOBIO_017}
#'           Vegetation composition and/or \code{SOBIO_018} Acoustic recording).
#'       }
#'     \item A legend for iconic taxon groups.
#'     \item A geoprivacy legend control (top right).
#'     \item A layers control to toggle observation groups and site boundary.
#'   }
#'
#' @note
#' Observations with \code{taxon_geoprivacy == "private"} are shown on the
#' map with low opacity as their coordinates are approximate.
#'
#' EU directive links point to the EUR-Lex PDF versions:
#' Birds Directive 2009/147/EC and Habitats Directive 92/43/EEC.
#'
#' eLTER Standard Observation assignments are based on taxonomic ancestry
#' retrieved from the iNaturalist API. SOBIO_014 covers Insecta;
#' SOBIO_018 covers Aves, Anura, Chiroptera, and Orthoptera. Orthoptera
#' contribute to both SOs simultaneously. SOBIO_017 covers Plants.
#'
#' @author Alessandro Oggioni, PhD \email{alessandro.oggioni@@cnr.it}
#' @author Alice Lenzi, PhD \email{alice.lenzi@@crea.gov.it}
#' @author Alessandro Campanaro, PhD \email{alessandro.campanaro@@crea.gov.it}
#'
#' @seealso
#' \code{\link{add_iucn_to_occ}},
#' \code{\link{add_nativeness_to_occ}},
#' \code{\link{add_eunis_legal_to_occ}},
#' \code{\link{obs_SO_pie_chart}}
#'
#' @export
#'
#' @importFrom dplyr coalesce mutate filter select distinct group_by
#' @importFrom dplyr summarise left_join case_when
#' @importFrom purrr map_chr pmap_chr map2_chr
#' @importFrom sf st_drop_geometry
#' @importFrom leaflet leaflet addTiles addPolygons addCircleMarkers
#' @importFrom leaflet addLegend addControl addLayersControl markerClusterOptions colorFactor
#' @importFrom leaflet layersControlOptions
#' @importFrom grDevices hcl.colors
#'
#' @examples
#' \dontrun{
#' # Download taxa occurrences from iNaturalist using ReLTER's
#' # get_site_speciesOccurrences() function
#' # e.g. Montagna di Torricchio eLTER site
#' deimsid <- "https://deims.org/6b62feb2-61bf-47e1-b97f-0e909c408db8"
#' site_boundary <- ReLTER::get_site_info(deimsid = deimsid)
#'
#' iNat_occ_eLTER_site <- ReLTER::get_site_speciesOccurrences(
#'  deimsid = deimsid,
#'  list_DS = "inat",
#'  show_map = TRUE,
#'  limit = 50
#' )
#'
#' occ_eLTER_enrich <- add_iucn_to_occ(
#'   occ_eLTER = iNat_occ_eLTER_site$inat
#' ) |>
#'   add_nativeness_to_occ(
#'     country = site_boundary$country
#'   ) |>
#'   add_eunis_legal_to_occ()
#'
#' map <- create_leaflet_occ_map(
#'   occ_enriched = occ_eLTER_enrich,
#'   site_boundary = site_boundary
#' )
#' map
#' }
#'
### create_leaflet_occ_map
create_leaflet_occ_map <- function(occ_enriched, site_boundary = NULL) {
  
  required_cols <- c(
    "status_IUCN",
    "establishmentMeans",
    "directive", "annex",
    "taxon.id", "name",
    "taxon.preferred_common_name", "taxon.iconic_taxon_name", "taxon_geoprivacy",
    "quality_grade", "observed_on", "public_positional_accuracy",
    "user.login", "uri", "taxon.default_photo.square_url",
    "SOBIO_014", "SOBIO_017", "SOBIO_018"
  )
  
  missing_cols <- setdiff(required_cols, names(occ_enriched))
  if (length(missing_cols) > 0) {
    stop("The following required columns are missing: ", paste(missing_cols, collapse = ", "))
  }
  
  iucn_labels <- c(
    "EX" = "Extinct",
    "EW" = "Extinct in the Wild",
    "CR" = "Critically Endangered",
    "EN" = "Endangered",
    "VU" = "Vulnerable",
    "NT" = "Near Threatened",
    "LC" = "Least Concern",
    "DD" = "Data Deficient",
    "NE" = "Not Evaluated"
  )
  
  # Helper: build HTML block for IUCN status from nested status_IUCN tibble
  # Shows one row per geographic scope (global + regional)
  extract_iucn_html <- function(iucn_tbl) {
    if (is.null(iucn_tbl) || nrow(iucn_tbl) == 0) return("-")
    rows <- purrr::pmap_chr(iucn_tbl, function(...) {
      args <- list(...)
      status <- args[["status"]]
      authority <- args[["authority"]]
      scope_of_assesment <- args[["scope_of_assesment"]]
      url <- args[["url"]]
      scope <- if (is.na(scope_of_assesment) || scope_of_assesment == "") "-" else scope_of_assesment
      label <- if (is.na(iucn_labels[status]) || iucn_labels[status] == "") "-" else iucn_labels[status]
      # label <- if (is.na(label)) status else paste0(status, " \u2013 ", label)
      if (!is.na(url) && nzchar(url)) {
        paste0(
          "<div style='font-size:12px;'>",
          scope, ": ",
          sprintf('<a href="%s" target="_blank">%s</a>', url, label),
          "</div>"
        )
      } else {
        paste0("<div style='font-size:12px;'>", scope, ": ", label, "</div>")
      }
    })
    paste(rows, collapse = "")
  }
  
  # Create map-ready labels, unpacking nested columns
  occ_map <- occ_enriched |>
    dplyr::mutate(
      iconic = dplyr::coalesce(taxon.iconic_taxon_name, "Unknown"),
      geopriv = dplyr::coalesce(taxon_geoprivacy, "unknown"),
      # Unpack establishmentMeans list-column
      nativeness_lbl = vapply(establishmentMeans, function(em) {
        val <- tryCatch(em$iNat_nativeness[[1]], error = function(e) NA_character_)
        if (is.null(val) || is.na(val)) "-" else as.character(val)
      }, FUN.VALUE = character(1)),
      em_authority_lbl = vapply(establishmentMeans, function(em) {
        val <- tryCatch(em$iNat_authority[[1]], error = function(e) NA_character_)
        if (is.null(val) || is.na(val)) "-" else as.character(val)
      }, FUN.VALUE = character(1)),
      em_authority_uri_lbl = vapply(establishmentMeans, function(em) {
        val <- tryCatch(em$iNat_checkList_uri[[1]], error = function(e) NA_character_)
        if (is.null(val) || is.na(val)) "-" else as.character(val)
      }, FUN.VALUE = character(1)),
      # Unpack status_IUCN list-column into an HTML string
      iucn_html = purrr::map_chr(status_IUCN, extract_iucn_html),
      # Flat columns
      obs_quality_lbl = ifelse(is.na(quality_grade), "-", as.character(quality_grade)),
      taxon_name_lbl = ifelse(is.na(name), "-", as.character(name)),
      common_name_lbl = ifelse(is.na(taxon.preferred_common_name), "-", as.character(taxon.preferred_common_name)),
      observed_on_lbl = ifelse(is.na(observed_on), "-", as.character(observed_on)),
      posacc_lbl = ifelse(is.na(public_positional_accuracy), "-", as.character(public_positional_accuracy)),
      observer_lbl = ifelse(is.na(user.login), "-", as.character(user.login)),
      observer_url = ifelse(is.na(user.login), "-", paste0("https://www.inaturalist.org/people/", user.login)),
      taxon_url = paste0("https://www.inaturalist.org/taxa/", taxon.id),
      obs_url = ifelse(is.na(uri), "", uri),
      photo_url = ifelse(is.na(taxon.default_photo.square_url), "", taxon.default_photo.square_url),
      # SOs
      so_lbl = apply(
        cbind(
          ifelse(SOBIO_014, "SOBIO_014 (Flying insects)", NA),
          ifelse(SOBIO_017, "SOBIO_017 (Vegetation composition)", NA),
          ifelse(SOBIO_018, "SOBIO_018 (Acoustic recording)", NA)
        ),
        1,
        function(x) {
          active <- x[!is.na(x)]
          if (length(active) == 0) "-" else paste(active, collapse = " | ")
        }
      ),
      # Unpack EASIN fields from nested establishmentMeans
      EASIN_url_lbl = vapply(establishmentMeans, function(em) {
        val <- tryCatch(em$EASIN_url[[1]], error = function(e) NA_character_)
        if (is.null(val) || is.na(val)) "-" else as.character(val)
      }, FUN.VALUE = character(1)),
      EASIN_id_lbl = vapply(establishmentMeans, function(em) {
        val <- tryCatch(em$EASIN_id[[1]], error = function(e) NA_character_)
        if (is.null(val) || is.na(val)) "-" else as.character(val)
      }, FUN.VALUE = character(1)),
      EASIN_status_lbl = vapply(establishmentMeans, function(em) {
        val <- tryCatch(em$EASIN_status[[1]], error = function(e) NA_character_)
        if (is.null(val) || is.na(val)) "-" else as.character(val)
      }, FUN.VALUE = character(1)),
      EASIN_impact_lbl = vapply(establishmentMeans, function(em) {
        val <- tryCatch(em$EASIN_hasImpact[[1]], error = function(e) NA_character_)
        if (is.null(val) || is.na(val)) "-" else as.character(val)
      }, FUN.VALUE = character(1)),
      EASIN_concern_lbl = vapply(establishmentMeans, function(em) {
        val <- tryCatch(em$EASIN_IsEUConcern[[1]], error = function(e) NA_character_)
        if (is.null(val) || is.na(val)) "-" else as.character(val)
      }, FUN.VALUE = character(1)),
      EASIN_intro_year_lbl = vapply(establishmentMeans, function(em) {
        val <- tryCatch(em$EASIN_firstIntroductionsInEU_year[[1]], error = function(e) NA_character_)
        if (is.null(val) || is.na(val)) "-" else as.character(val)
      }, FUN.VALUE = character(1)),
      EASIN_intro_country_lbl = purrr::map_chr(
        vapply(establishmentMeans, function(em) {
        val <- tryCatch(em$EASIN_firstIntroductions_Country[[1]], error = function(e) NA_character_)
        if (is.null(val) || is.na(val)) "-" else as.character(val)
        }, FUN.VALUE = character(1)),
        .country_to_flag
      )
    )
  
  # Collapse directive + Annex per taxon.id into one HTML block (one row per directive)
  directive_html_tbl <- occ_map |>
    sf::st_drop_geometry() |>
    dplyr::select(taxon.id, directive, annex) |>
    dplyr::distinct() |>
    dplyr::group_by(taxon.id) |>
    dplyr::summarise(
      directive_html = {
        dirs    <- directive
        annexes <- annex
        parts <- purrr::map2_chr(dirs, annexes, function(d, a) {
          if (is.na(d)) return("")
          annex_str <- if (!is.na(a) && nzchar(a)) paste0("<br/><b>Annex</b> - ", a) else ""
          paste0("<div style='font-size:12px;'>", d, annex_str, ".</div>")
        })
        paste(parts[nzchar(parts)], collapse = "")
      },
      .groups = "drop"
    )
  
  occ_map <- occ_map |>
    dplyr::left_join(directive_html_tbl, by = "taxon.id")
  
  # Build popups
  occ_map$popup <- purrr::pmap_chr(
    list(
      obs_quality = occ_map$obs_quality_lbl,
      taxon_name = occ_map$taxon_name_lbl,
      taxon_url = occ_map$taxon_url,
      iucn_html = occ_map$iucn_html,
      nativeness = occ_map$nativeness_lbl,
      em_authority = occ_map$em_authority_lbl,
      em_authority_uri = occ_map$em_authority_uri_lbl,
      common_name = occ_map$common_name_lbl,
      observed_on = occ_map$observed_on_lbl,
      geopriv = occ_map$geopriv,
      posacc = occ_map$posacc_lbl,
      observer = occ_map$observer_lbl,
      observer_url = occ_map$observer_url,
      obs_url = occ_map$obs_url,
      photo_url = occ_map$photo_url,
      directive_html = occ_map$directive_html,
      so_lbl = occ_map$so_lbl,
      EASIN_url = occ_map$EASIN_url_lbl,
      EASIN_id = occ_map$EASIN_id_lbl,
      EASIN_status = occ_map$EASIN_status_lbl,
      EASIN_impact = occ_map$EASIN_impact_lbl,
      EASIN_concern = occ_map$EASIN_concern_lbl,
      EASIN_intro_year = occ_map$EASIN_intro_year_lbl,
      EASIN_intro_country = occ_map$EASIN_intro_country_lbl
    ),
    function(obs_quality, taxon_name, taxon_url, iucn_html, nativeness, em_authority,
             em_authority_uri,
             common_name, observed_on, geopriv, posacc,
             observer, observer_url, obs_url, photo_url, directive_html, so_lbl,
             EASIN_url, EASIN_id, EASIN_status, EASIN_impact, EASIN_concern,
             EASIN_intro_year, EASIN_intro_country) {
      
      # Build directive HTML block with links to EUR-Lex
      directive_block <- if (nzchar(directive_html)) {
        # Inject EUR-Lex hyperlink based on directive type
        linked_html <- gsub(
          "EU Birds Directive[^<]*",
          '<a href="https://eur-lex.europa.eu/legal-content/EN/TXT/PDF/?uri=CELEX:32009L0147" target="_blank">EU Birds Directive (2009/147/EC)</a>.',
          directive_html
        )
        linked_html <- gsub(
          "EU Habitats Directive[^<]*",
          '<a href="https://eur-lex.europa.eu/legal-content/EN/TXT/PDF/?uri=CELEX:31992L0043" target="_blank">EU Habitats Directive (92/43/EEC)</a>.',
          linked_html
        )
        paste0("<b>EU Directives</b>: ", linked_html)
      } else ""
      
      
      paste0(
        # --- Photo ---
        if (nzchar(photo_url))
          sprintf(
            '<div style="margin-bottom:8px;text-align:center;">
         <img src="%s" style="width:110px;height:auto;border-radius:8px;">
       </div>', photo_url)
        else "",
        
        # --- Section: iNaturalist observation data ---
        '<div style="background:#f0f7f0;border-left:3px solid #74ac00;
               padding:6px 8px;margin-bottom:6px;border-radius:0 4px 4px 0;">',
        '<div style="display:flex;align-items:center;gap:6px;margin-bottom:4px;">',
        '<img src="https://static.inaturalist.org/wiki_page_attachments/1419-original.png"
        style="height:18px;width:auto;vertical-align:middle;">',
        '<span style="font-size:11px;color:#555;font-weight:500;">iNaturalist observation</span>',
        '</div>',
        "<b>Taxon:</b> ",
        if (taxon_name != "-")
          sprintf('<a href="%s" target="_blank"><i>%s</i></a>', taxon_url, taxon_name)
        else "-",
        "<br/><b>Common name:</b> ", common_name, "<br/>",
        "<b>Observed on:</b> ", observed_on, "<br/>",
        "<b>Observer:</b> ",
        if (observer != "-")
          sprintf('<a href="%s" target="_blank">%s</a>', observer_url, observer)
        else "-",
        "<br/>",
        if (nzchar(obs_url))
          sprintf('<b>Observation:</b> <a href="%s" target="_blank">open record</a><br/>', obs_url)
        else "",
        '<div style="margin-top:4px;">',
        "<b>Obs quality:</b> ", obs_quality,
        sprintf(' <a href="https://help.inaturalist.org/en/support/solutions/articles/151000169936"
              target="_blank" style="font-size:11px;">ℹ️</a><br/>'),
        sprintf('<b>Geoprivacy:</b> %s
           <a href="https://www.inaturalist.org/pages/geoprivacy"
              target="_blank" style="font-size:11px;">ℹ️</a><br/>', geopriv),
        "<b>Positional accuracy (m):</b> ", posacc,
        '</div>',
        '</div>',
        
        # --- Section: IUCN Red List (via iNaturalist) ---
        if (grepl('-: -', iucn_html)) {
          paste0(
            '<div style="background:#fff8f0;border-left:3px solid #e8401c;
                 padding:6px 8px;margin-bottom:6px;border-radius:0 4px 4px 0;">',
            '<div style="display:flex;align-items:center;gap:6px;margin-bottom:4px;">',
            '<img src="https://upload.wikimedia.org/wikipedia/en/thumb/e/ec/IUCN_Red_List.svg/330px-IUCN_Red_List.svg.png"
          style="height:18px;width:auto;vertical-align:middle;">',
            '<span style="font-size:11px;color:#555;font-weight:500;">IUCN Red List
      <a href="https://forum.inaturalist.org/t/updating-iucn-red-list-conservation-statuses/25712"
         target="_blank" style="font-size:11px;">ℹ️</a>
      <span style="font-weight:300;font-style:italic;">(via iNaturalist, provided by IUCN)</span>
    </span>',
            '</div>',
            'This taxon has no IUCN Red List status recorded in iNaturalist',
            '</div>'
          )
        } else {
          paste0(
            '<div style="background:#fff8f0;border-left:3px solid #e8401c;
                 padding:6px 8px;margin-bottom:6px;border-radius:0 4px 4px 0;">',
            '<div style="display:flex;align-items:center;gap:6px;margin-bottom:4px;">',
            '<img src="https://upload.wikimedia.org/wikipedia/en/thumb/e/ec/IUCN_Red_List.svg/330px-IUCN_Red_List.svg.png"
          style="height:18px;width:auto;vertical-align:middle;">',
            '<span style="font-size:11px;color:#555;font-weight:500;">IUCN Red List
      <a href="https://forum.inaturalist.org/t/updating-iucn-red-list-conservation-statuses/25712"
         target="_blank" style="font-size:11px;">ℹ️</a>
      <span style="font-weight:300;font-style:italic;">(via iNaturalist, provided by IUCN)</span>
    </span>',
            '</div>',
            iucn_html,
            '</div>'
          )
        },
        
        # --- Section: EASIN ---
        if (EASIN_id == "-") {
          paste0(
            '<div style="background:#fff5f5;border-left:3px solid #c00000;
                 padding:6px 8px;margin-bottom:6px;border-radius:0 4px 4px 0;">',
            '<div style="display:flex;align-items:center;gap:6px;margin-bottom:4px;">',
            '<img src="https://easin.jrc.ec.europa.eu/easin/Content/ECL/images/logo/positive/logo-ec--en.svg"
          style="height:18px;width:auto;vertical-align:middle;">',
            '<span style="font-size:11px;color:#555;font-weight:500;">EASIN
      <span style="font-weight:300;font-style:italic;">
        — European Alien Species Information Network
      </span><a href="https://easin.jrc.ec.europa.eu/easin"
       target="_blank" style="font-size:11px;">ℹ️</a>
    </span>',
            '</div>',
            'This taxon is not present in EASIN database',
            '</div>'
          )
        } else {
          paste0(
            '<div style="background:#fff5f5;border-left:3px solid #c00000;
                 padding:6px 8px;margin-bottom:6px;border-radius:0 4px 4px 0;">',
            '<div style="display:flex;align-items:center;gap:6px;margin-bottom:4px;">',
            '<img src="https://easin.jrc.ec.europa.eu/easin/Content/ECL/images/logo/positive/logo-ec--en.svg"
          style="height:18px;width:auto;vertical-align:middle;">',
            '<span style="font-size:11px;color:#555;font-weight:500;">EASIN
      <span style="font-weight:300;font-style:italic;">
        — European Alien Species Information Network
      </span><a href="https://easin.jrc.ec.europa.eu/easin"
       target="_blank" style="font-size:11px;">ℹ️</a>
    </span>',
            '</div>',
            dplyr::case_when(
              EASIN_url == "-" ~ '<b>Species:</b> <span style="color:#999;">-</span><br/>',
              TRUE ~ sprintf('<b>Species:</b> <a href="%s" target="_blank">factsheet</a><br/>', EASIN_url)
            ),
            dplyr::case_when(
              EASIN_id == "-" ~ '<b>LSID:</b> <span style="color:#999;">-</span><br/>',
              TRUE ~ sprintf('<b>LSID:</b> <span style="font-size:11px;">urn:lsid:easin.jrc.ec.europa.eu:species:%s</span><br/>', EASIN_id)
            ),
            paste0(
              dplyr::case_when(
                EASIN_status == "A" ~ '<b>Status:</b> Alien - species introduced outside its native range ',
                EASIN_status == "Q" ~ '<b>Status:</b> Cryptogenic - species with unknown native range or pathway of introduction ',
                EASIN_status == "C" ~ '<b>Status:</b> Questionable - species with unresolved taxonomic status or not verified by experts ',
                EASIN_id == "-" ~ '<b>Status:</b> - '
              ),
              '<a href="https://easin.jrc.ec.europa.eu/easin/Catalogue/Protocol" target="_blank" style="font-size:11px;">ℹ️</a><br/>'
            ),
            dplyr::case_when(
              EASIN_impact == "FALSE" ~ '<b>Has documented impact:</b> No <br/>',
              EASIN_impact == "TRUE" ~ '<b>Has documented impact:</b> Yes <br/>',
              EASIN_id == "-" ~ '<b>Has documented impact:</b> - </br>'
            ),
            dplyr::case_when(
              EASIN_concern == "FALSE" ~ '<b>Species of EU concern:</b> No <br/>',
              EASIN_concern == "TRUE" ~ '<b>Species of EU concern:</b> Yes <br/>',
              EASIN_concern == "-" ~ '<b>Species of EU concern:</b> - </br>'
            ),
            sprintf('<b>First introduction in EU:</b> %s in %s<br/>',
                    EASIN_intro_year, EASIN_intro_country),
            '</div>'
          )
        },
        
        # --- Section: Establishment means (iNaturalist) ---
        # --- Section: Establishment means (iNaturalist) ---
        if (nativeness == "-" && em_authority == "-") {
          paste0(
            '<div style="background:#f0f7f0;border-left:3px solid #74ac00;
                 padding:6px 8px;margin-bottom:6px;border-radius:0 4px 4px 0;">',
            '<div style="display:flex;align-items:center;gap:6px;margin-bottom:4px;">',
            '<img src="https://static.inaturalist.org/wiki_page_attachments/1419-original.png"
          style="height:18px;width:auto;vertical-align:middle;">',
            '<span style="font-size:11px;color:#555;font-weight:500;">Establishment means
      <a href="https://help.inaturalist.org/en/support/solutions/articles/151000176171"
         target="_blank" style="font-size:11px;">ℹ️</a>
      <span style="font-weight:300;font-style:italic;">(via iNaturalist)</span>
    </span>',
            '</div>',
            'No establishment means recorded in iNaturalist for this taxon',
            '</div>'
          )
        } else {
          paste0(
            '<div style="background:#f0f7f0;border-left:3px solid #74ac00;
                 padding:6px 8px;margin-bottom:6px;border-radius:0 4px 4px 0;">',
            '<div style="display:flex;align-items:center;gap:6px;margin-bottom:4px;">',
            '<img src="https://static.inaturalist.org/wiki_page_attachments/1419-original.png"
          style="height:18px;width:auto;vertical-align:middle;">',
            '<span style="font-size:11px;color:#555;font-weight:500;">Establishment means
      <a href="https://help.inaturalist.org/en/support/solutions/articles/151000176171"
         target="_blank" style="font-size:11px;">ℹ️</a>
      <span style="font-weight:300;font-style:italic;">(via iNaturalist)</span>
    </span>',
            '</div>',
            sprintf('<b>Establishment means:</b> %s<br/>', nativeness),
            dplyr::case_when(
              em_authority_uri == "-" ~ '<b>Establishment means authority:</b> -<br/>',
              TRUE ~ sprintf(
                '<b>Establishment means authority:</b> <a href="%s" target="_blank">%s</a><br/>',
                em_authority_uri, em_authority
              )
            ),
            '</div>'
          )
        },
        
        # --- Section: EU Directives (EUNIS) ---
        if (nzchar(directive_block)) {
          paste0(
            '<div style="background:#f0f4ff;border-left:3px solid #004b87;
                   padding:6px 8px;margin-bottom:6px;border-radius:0 4px 4px 0;">',
            '<div style="display:flex;align-items:center;gap:6px;margin-bottom:4px;">',
            '<img src="https://www.eea.europa.eu/static/media/eea-logo.55fb94a4.svg"
            style="height:18px;width:auto;vertical-align:middle;">',
            '<span style="font-size:11px;color:#555;font-weight:500;">EU Legal framework
        <span style="font-weight:300;font-style:italic;">(via EUNIS)</span>
      </span>',
            '</div>',
            directive_block,
            '</div>'
          )
        } else {
          paste0(
            '<div style="background:#f0f4ff;border-left:3px solid #004b87;
                   padding:6px 8px;margin-bottom:6px;border-radius:0 4px 4px 0;">',
            '<div style="display:flex;align-items:center;gap:6px;margin-bottom:4px;">',
            '<img src="https://www.eea.europa.eu/static/media/eea-logo.55fb94a4.svg"
            style="height:18px;width:auto;vertical-align:middle;">',
            '<span style="font-size:11px;color:#555;font-weight:500;">EU Legal framework
        <span style="font-weight:300;font-style:italic;">(via EUNIS)</span>
      </span>',
            '</div>',
            'This taxon is not included in any Directives',
            '</div>'
          )
        },
        
        # --- Section: eLTER Standard Observations ---
        if (so_lbl == "-") {
          paste0('<div style="background:#f5f0ff;border-left:3px solid #000F22;
               padding:6px 8px;margin-bottom:2px;border-radius:0 4px 4px 0;">',
          '<div style="display:flex;align-items:center;gap:6px;margin-bottom:4px;">',
          '<img src="https://elter-ri.eu/media-center/logo?&download=131&file_name=eLTER_Logo"
        style="height:18px;width:auto;vertical-align:middle;background:white;
               padding:2px 4px;border-radius:3px;">',
          '<span style="font-size:11px;color:#555;font-weight:500;">eLTER Standard Observations (SOs)
    <a href="https://elter-ri.eu/standard-observations-spheres/biosphere"
       target="_blank" style="font-size:11px;">ℹ️</a>
  </span>',
          '</div>',
          "<div style='font-size:12px;'>This observation can't contribute to any eLTER SOs ", 
          "</div>",
          '</div>')
        } else {
          paste0('<div style="background:#f5f0ff;border-left:3px solid #000F22;
               padding:6px 8px;margin-bottom:2px;border-radius:0 4px 4px 0;">',
          '<div style="display:flex;align-items:center;gap:6px;margin-bottom:4px;">',
          '<img src="https://elter-ri.eu/media-center/logo?&download=131&file_name=eLTER_Logo"
        style="height:18px;width:auto;vertical-align:middle;background:white;
               padding:2px 4px;border-radius:3px;">',
          '<span style="font-size:11px;color:#555;font-weight:500;">eLTER Standard Observations (SOs)
    <a href="https://elter-ri.eu/standard-observations-spheres/biosphere"
       target="_blank" style="font-size:11px;">ℹ️</a>
  </span>',
          '</div>',
          "<div style='font-size:12px;'>This observation can contribute to: ", 
          so_lbl, "</div>",
          '</div>')
        }
      )
    }
  )
  
  occ_open     <- occ_map |> dplyr::filter(geopriv == "open")
  occ_obscured <- occ_map |> dplyr::filter(geopriv == "obscured")
  occ_private  <- occ_map |> dplyr::filter(!geopriv %in% c("open", "obscured"))
  
  iconic_levels <- sort(unique(occ_map$iconic))
  pal <- leaflet::colorFactor(
    palette = grDevices::hcl.colors(length(iconic_levels), "Set 2"),
    domain  = occ_map$iconic
  )
  cluster_opts <- leaflet::markerClusterOptions(
    spiderfyOnMaxZoom    = TRUE,
    showCoverageOnHover  = FALSE,
    zoomToBoundsOnClick  = TRUE
  )
  
  # Build the Leaflet map
  map <- leaflet::leaflet() |>
    leaflet::addTiles()
  
  if (!is.null(site_boundary)) {
    map <- map |>
      leaflet::addPolygons(
        data         = site_boundary,
        color        = "white",
        weight       = 4,
        opacity      = 1,
        fillColor    = "#D8A24A",
        fillOpacity  = 0.45,
        smoothFactor = 0.5,
        popup = ~paste0(
          "<b>Site title:</b><br>",
          sprintf('<a href="%s" target="_blank">%s</a>', uri, title.x)
        ),
        group = "eLTER site boundary"
      )
  }
  
  map <- map |>
    leaflet::addCircleMarkers(
      data        = occ_open,
      radius      = 6, fill = TRUE, color = "white", weight = 3,
      fillColor   = ~pal(iconic), fillOpacity = 1,
      popup       = ~popup, group = "open", clusterOptions = cluster_opts
    ) |>
    leaflet::addCircleMarkers(
      data        = occ_obscured,
      radius      = 10, fill = TRUE, color = "white", weight = 3,
      fillColor   = ~pal(iconic), fillOpacity = 0.35,
      popup       = ~popup, group = "obscured", clusterOptions = cluster_opts
    ) |>
    leaflet::addCircleMarkers(
      data        = occ_private,
      radius      = 8, fill = TRUE, color = "white", weight = 3,
      fillColor   = ~pal(iconic), fillOpacity = 0.2,
      popup       = ~popup, group = "unknown", clusterOptions = cluster_opts
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      pal      = pal,
      values   = occ_map$iconic,
      title    = "Iconic taxon"
    ) |>
    leaflet::addControl(
      html = paste0(
        "<div style='background:white; padding:8px 10px; border-radius:4px;'>",
        "<b>Geoprivacy</b><br/>",
        "<svg width='18' height='18' style='vertical-align:middle; margin-right:4px;'>",
        "<circle cx='9' cy='9' r='4' fill='gray' fill-opacity='1' stroke='black' stroke-width='1' /></svg> open<br/>",
        "<svg width='22' height='22' style='vertical-align:middle; margin-right:4px;'>",
        "<circle cx='11' cy='11' r='7' fill='gray' fill-opacity='0.35' stroke='black' stroke-width='2' /></svg> obscured<br/>",
        "<svg width='18' height='18' style='vertical-align:middle; margin-right:4px;'>",
        "<circle cx='9' cy='9' r='4' fill='gray' fill-opacity='0.2' stroke='black' stroke-width='1' /></svg> unknown",
        "</div>"
      ),
      position = "topright"
    ) |>
    leaflet::addLayersControl(
      overlayGroups = c("open", "obscured", "unknown"),
      options       = leaflet::layersControlOptions(collapsed = FALSE)
    )
  overlay_groups <- c("open", "obscured", "unknown")
  if (!is.null(site_boundary)) {
    overlay_groups <- c(overlay_groups, "eLTER site boundary")
  }
  
  map <- map |>
    leaflet::addLayersControl(
      overlayGroups = overlay_groups,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )
  return(map)
}
