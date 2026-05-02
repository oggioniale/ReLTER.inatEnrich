#' Function to obtain IUCN conservation status for a single taxon.id from
#' iNaturalist API
#' @description `r lifecycle::badge("experimental")`
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
#' @author Alessandro Oggioni, phD (2023) \email{alessandro.oggioni@@cnr.it}
#' @importFrom httr2 request req_method req_headers req_retry req_perform resp_status resp_body_json
#' @importFrom dplyr tibble select
#' @export
#' @examples
#' \dontrun{
#' ## Not run:
#' get_conservation_status(
#'   taxon.id = 517449
#' )
#' 
#' get_conservation_status(
#'   taxon.id = 632126
#' )
#' 
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
      name = NA_character_,
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
    p <- cons_status$place[[i]]
    if (is.null(p))                                return(NA_character_)
    if (is.data.frame(p) && "name" %in% names(p)) return(as.character(p$name[1]))
    if (is.list(p) && !is.null(p[["name"]]))       return(as.character(p[["name"]][1]))
    NA_character_
  }, FUN.VALUE = character(1))
  
  return(tibble::tibble(
    status    = as.character(cons_status$status),
    authority = as.character(cons_status$authority),
    name      = place_name,
    url       = as.character(cons_status$url)
  ))
}

#' Get nativeness degree for a taxon from iNaturalist
#' @description `r lifecycle::badge("experimental")`
#' Queries the iNaturalist API to retrieve the establishment means
#' (nativeness status and authority) for a given taxon, optionally filtered
#' by country. Returns a one-row tibble with a nested \code{establishmentMeans}
#' list-column containing \code{nativeness} and \code{authority}.
#' @param taxon.id \code{integer} or \code{character}. The iNaturalist taxon ID
#'   to query.
#' @param country \code{character}. The country name to filter results by
#'   (e.g., \code{"Italy"}). Must match the place name as recorded in
#'   iNaturalist. If \code{NULL}, a warning is issued and an empty result
#'   is returned to avoid ambiguous cross-country data.
#' @return A \code{\link[dplyr]{tibble}} with one row and one list-column:
#'   \describe{
#'     \item{establishmentMeans}{\code{list} of one-row tibbles, each containing:
#'       \describe{
#'         \item{nativeness}{\code{character}. The establishment means value
#'           (e.g., \code{"native"}, \code{"introduced"}), or \code{NA} if
#'           not available.}
#'         \item{authority}{\code{character}. The authority or checklist title
#'           associated with the establishment means (e.g.,
#'           \code{"Italy Check List"}), or \code{NA} if not available.}
#'       }
#'     }
#'   }
#' @note The establishment means information is sourced from iNaturalist and
#'   may refer to the IUCN Red List. It may not always be up to date.
#' @seealso \code{\link{add_nativeness_to_occ}} for applying this function
#'   across a full occurrence tibble.
#' @author Alessandro Oggioni, PhD (2023) \email{alessandro.oggioni@@cnr.it}
#' @importFrom httr2 request req_method req_headers req_retry req_perform
#'   resp_status resp_body_json
#' @importFrom dplyr tibble
#' @importFrom purrr keep
#' @examples
#' \dontrun{
#' # Get nativeness for a taxon in Italy
#' get_nativeness_degree(taxon.id = 48484, country = "Italy")
#' }
#'
### get_nativeness_degree
get_nativeness_degree <- function(taxon.id, country = NULL) {
  
  # Helper: nested tibble returned in case of error or missing data
  empty_nested <- dplyr::tibble(
    establishmentMeans = list(
      dplyr::tibble(
        nativeness = NA_character_,
        authority  = NA_character_
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
  authority_value  <- record$list$title
  
  # Return a tibble with establishmentMeans as a nested list-column
  dplyr::tibble(
    establishmentMeans = list(
      dplyr::tibble(
        nativeness = if (is.null(nativeness_value)) NA_character_ else as.character(nativeness_value),
        authority  = if (is.null(authority_value))  NA_character_ else as.character(authority_value)
      )
    )
  )
}

#' Get EUNIS Legal Information for a Species Using iNaturalist Taxon ID
#' @description `r lifecycle::badge("experimental")`
#' This function takes a \code{taxon.id} from iNaturalist, retrieves the corresponding
#' scientific name from the iNaturalist API, searches the EUNIS database, and extracts
#' the legal information related to the EU Habitats Directive (92/43/EEC) and Birds Directive (2009/147/EC).
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
#' @author Alessandro Oggioni, PhD (2023) \email{alessandro.oggioni@@cnr.it}
#'
#' @importFrom httr2 request req_method req_headers req_retry req_perform
#'   resp_status resp_body_json
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
#' @author Alessandro Oggioni, phD (2023) \email{alessandro.oggioni@@cnr.it}
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
#' @description `r lifecycle::badge("experimental")`
#' This function enriches all the eLTER site iNaturalist occurrences acquired
#' by the `ReLTER::get_site_speciesOccurrences()` function with the IUCN Red
#' List conservation status as recorded in iNaturalist.
#' 
#' Observations are filtered to include only "Research Grade" data that meet
#' the following criteria: have a valid date and exclude captive or cultivated
#' organisms.
#' 
#' Observations are not filtered with respect to geoprivacy. In iNaturalist,
#' each observation can be assigned one of three geoprivacy levels: open,
#' obscured, or private (for more information, see:
#' https://www.inaturalist.org/pages/geoprivacy). Given the need to gather
#' information on species with critical conservation status, even if the
#' geographic information is not precise, it is still important to record
#' the presence of a species within a given area.
#' 
#' The function queries the iNaturalist Taxa API once per unique `taxon.id`
#' and stores the returned IUCN assessments (if available) in a nested
#' list-column.
#' @param occ_eLTER A `data.frame` or `sf` object representing the occurrences
#' of an eLTER site acquired by the
#' `ReLTER::get_site_speciesOccurrences()` function (typically `occ_eLTER$inat`).
#' @return A `data.frame` or `sf` object with the same structure as the input,
#' enriched with the following additional columns:
#' \describe{
#'   \item{status_IUCN}{A list-column (`list` of `tibble`s) containing the IUCN
#'   Red List assessments retrieved from iNaturalist for each `taxon.id`.
#'   Each tibble has the following columns: `status`, `authority`, `name`, `url`.
#'   If no IUCN information is available, a tibble with `NA` values is stored.}
#'   \item{has_IUCN}{A `logical` value indicating whether at least one valid
#'   (non-`NA`) IUCN status is available for the given `taxon.id`.}
#' }
#' 
#' The function also prints informative console messages reporting:
#' \itemize{
#'   \item the progress of API queries for each `taxon.id`
#'   \item whether IUCN data are available or missing
#'   \item a summary of taxa with and without IUCN status
#' }
#' @details
#' The IUCN conservation status is retrieved from the iNaturalist Taxa API and
#' reflects the information available within iNaturalist. This information may
#' not be up to date with respect to the official IUCN Red List.
#' @author Alessandro Oggioni, PhD (2023) \email{alessandro.oggioni@@cnr.it}
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
#'   limit = 5000
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
    status <- tryCatch(
      get_conservation_status(taxon.id),
      error = function(e) NULL
    )
    # Normalize output
    if (is.null(status)) {
      status_tbl <- empty_status_tbl
    } else if (inherits(status, "data.frame")) {
      if (nrow(status) == 0) {
        status_tbl <- empty_status_tbl
      } else {
        status_tbl <- status
      }
    } else {
      status_tbl <- empty_status_tbl
    }
    # Check validity
    valid <- has_valid_status(status_tbl)
    # Logging
    if (!valid) {
      message("⚠️ Progressive taxon record: ", record_counter,
              " | iNat taxon id: ", taxon.id,
              " | No IUCN status recorded in iNaturalist")
    } else {
      message("✅ Progressive: ", record_counter,
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
        dplyr::select(taxon.id, status_IUCN, has_IUCN),
      by = "taxon.id"
    )
  return(occ_in_site_research_valid)
}

#' Add nativeness information to iNaturalist occurrence records
#' @description `r lifecycle::badge("experimental")`
#' Filters a tibble of iNaturalist occurrence records to retain only
#' research-grade, non-captive observations with a valid date, then fetches
#' establishment means information from iNaturalist for each unique taxon
#' via \code{\link{get_nativeness_degree}}. The results are joined back to
#' the filtered occurrence tibble as a nested \code{establishmentMeans}
#' list-column.
#' @param occ_eLTER \code{\link[dplyr]{tibble}}. A tibble of iNaturalist
#'   occurrence records, typically obtained via
#'   \code{ReLTER::get_site_speciesOccurrences()}. Must contain at least the
#'   columns \code{quality_grade}, \code{observed_on}, \code{captive}, and
#'   \code{taxon.id}.
#' @param country \code{character}. The country name to filter establishment
#'   means by (e.g., \code{"Italy"}). Must match the place name as recorded
#'   in iNaturalist. Cannot be \code{NULL}.
#' @return A \code{\link[dplyr]{tibble}} of filtered occurrence records
#'   (research-grade, non-captive, with valid date) with an additional
#'   \code{establishmentMeans} list-column. Each element of the list-column
#'   is a one-row tibble containing:
#'   \describe{
#'     \item{nativeness}{\code{character}. The establishment means value
#'       (e.g., \code{"native"}, \code{"introduced"}), or \code{NA} if
#'       not recorded in iNaturalist for the specified country.}
#'     \item{authority}{\code{character}. The checklist or authority title
#'       associated with the establishment means, or \code{NA} if not
#'       available.}
#'   }
#' @note Progress messages are printed to the console for each taxon
#'   processed, including the iNaturalist taxon ID, nativeness status,
#'   and authority. A summary of taxa with and without establishment means
#'   is printed at the end.
#'
#'   The establishment means information is sourced from iNaturalist and
#'   may refer to the IUCN Red List. It may not always be up to date.
#' @seealso \code{\link{get_nativeness_degree}} for the underlying API call.
#' @author Alessandro Oggioni, PhD (2023) \email{alessandro.oggioni@@cnr.it}
#' @importFrom dplyr filter select distinct inner_join left_join tibble
#' @importFrom purrr map_dfr pluck walk2 map_chr
#' @export
#' @examples
#' \dontrun{
#' deimsid <- "https://deims.org/6b62feb2-61bf-47e1-b97f-0e909c408db8"
#'
#' occ_eLTER <- ReLTER::get_site_speciesOccurrences(
#'   deimsid = deimsid,
#'   list_DS = "inat",
#'   show_map = FALSE,
#'   limit = 5000
#' )
#'
#' site_boundary <- ReLTER::get_site_info(deimsid = deimsid)
#' country <- site_boundary$country
#'
#' occ <- add_nativeness_to_occ(
#'   occ_eLTER = occ_eLTER$inat,
#'   country = country
#' )
#' }
#'
### add_nativeness_to_occ
add_nativeness_to_occ <- function(occ_eLTER, country) {
  if (is.null(country)) {
    stop("Argument `country` is required. Please specify a country name (e.g., 'Italy').")
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
    nativeness_val <- purrr::pluck(raw, "establishmentMeans", 1, "nativeness", 1)
    authority_val  <- purrr::pluck(raw, "establishmentMeans", 1, "authority",  1)
    
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
      taxon.id           = taxon.id,
      establishmentMeans = raw$establishmentMeans
    )
  })
  
  # Helper to extract nativeness string from the list-column for filtering
  get_nat <- function(em_list) purrr::map_chr(em_list, ~ .x$nativeness %||% NA_character_)
  
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
    dplyr::left_join(nativeness_results, by = "taxon.id")
}

#' Enrich iNaturalist occurrences with EUNIS legal framework information
#' @description `r lifecycle::badge("experimental")`
#' This function enriches iNaturalist occurrences with legal protection
#' information extracted from the EUNIS database. It uses the scientific
#' name retrieved from the iNaturalist `taxon.id` to query EUNIS and extract
#' legal directives related to the EU Habitats Directive (92/43/EEC) and
#' Birds Directive (2009/147/EC).
#'
#' Observations are not filtered by geoprivacy or research grade. If a taxon
#' has no legal information in EUNIS, NA values are returned for `Legal text`
#' and `Annex`.
#' @param occ_eLTER A `tibble` containing iNaturalist occurrences. Must contain a column `taxon.id`.
#' @return A `tibble` containing all original columns of `occ_eLTER` plus:
#'   \describe{
#'     \item{`Legal text`}{Legal directive text from EUNIS (92/43/EEC or 2009/147/EC)}
#'     \item{Annex}{Annex information from EUNIS table}
#'   }
#' @author Alessandro Oggioni, PhD (2023) \email{alessandro.oggioni@@cnr.it}
#' @importFrom dplyr tibble mutate left_join rename
#' @importFrom purrr map_dfr
#' @export
#' @examples
#' \dontrun{
#' # Example: enrich iNaturalist occurrences with legal info
#' deimsid <- "https://deims.org/6b62feb2-61bf-47e1-b97f-0e909c408db8"
#' occ_iNat <- ReLTER::get_site_speciesOccurrences(
#'   deimsid = deimsid,
#'   list_DS = "inat",
#'   show_map = FALSE,
#'   limit = 5000
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
#' @description `r lifecycle::badge("experimental")`
#'
#' Takes a tibble of iNaturalist occurrences already enriched with species
#' nativeness, IUCN conservation status, and EUNIS legal information.
#' Verifies that all required columns are present, builds observation-level
#' HTML popups with linked references to iNaturalist, IUCN Red List, and
#' EUR-Lex directive pages, and returns an interactive Leaflet map with
#' colour-coded markers by iconic taxon group and marker clustering.
#'
#' @param occ_enriched An \code{sf} tibble of iNaturalist occurrences enriched
#'   with the following columns:
#'   \describe{
#'     \item{establishmentMeans}{list-column of 1 × 2 tibbles with
#'       \code{nativeness} and \code{authority}, produced by
#'       \code{\link{add_nativeness_to_occ}}.}
#'     \item{status_IUCN}{list-column of N × 4 tibbles with \code{status},
#'       \code{authority}, \code{name} (geographic scope), and \code{url},
#'       produced by \code{\link{add_iucn_to_occ}}.}
#'     \item{directive}{character. EU directive name, produced by
#'       \code{\link{add_eunis_legal_to_occ}}.}
#'     \item{annex}{character. EU directive annex, produced by
#'       \code{\link{add_eunis_legal_to_occ}}.}
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
#'
#' @return A \code{\link[leaflet]{leaflet}} map object with:
#'   \itemize{
#'     \item An OpenStreetMap base tile layer.
#'     \item The eLTER-RI site boundary polygon (retrieved via
#'       \code{ReLTER::get_site_info()}).
#'     \item Circle markers for \code{"open"} observations (radius 6, full
#'       opacity) and \code{"obscured"} observations (radius 10, reduced
#'       opacity), both with marker clustering enabled.
#'     \item Popups per observation showing taxon info, observer details,
#'       data quality, IUCN status per geographic scope (with links to IUCN
#'       Red List assessments), establishment means (with link to iNaturalist
#'       help), and EU directive coverage (with links to EUR-Lex).
#'     \item A legend for iconic taxon groups.
#'     \item A geoprivacy legend control (top right).
#'     \item A layers control to toggle \code{"open"} and \code{"obscured"}
#'       marker groups.
#'   }
#'
#' @note
#' The function calls \code{ReLTER::get_site_info()} internally to retrieve
#' the site boundary, using the DEIMS-ID stored in \code{occ_enriched$uri.1[1]}.
#' Observations with \code{taxon_geoprivacy == "private"} are silently
#' excluded from the map as they have no displayable coordinates.
#'
#' EU directive links point to the EUR-Lex PDF versions:
#' Birds Directive 2009/147/EC and Habitats Directive 92/43/EEC.
#'
#' @author Alessandro Oggioni, PhD \email{alessandro.oggioni@@cnr.it}
#'
#' @seealso
#' \code{\link{add_iucn_to_occ}},
#' \code{\link{add_nativeness_to_occ}},
#' \code{\link{add_eunis_legal_to_occ}}
#'
#' @export
#'
#' @importFrom dplyr coalesce mutate filter select distinct group_by
#'   summarise left_join
#' @importFrom purrr map_chr pmap_chr map2_chr
#' @importFrom sf st_drop_geometry
#' @importFrom leaflet leaflet addTiles addPolygons addCircleMarkers
#'   addLegend addControl addLayersControl markerClusterOptions colorFactor
#'   layersControlOptions
#' @importFrom grDevices hcl.colors
#'
#' @examples
#' \dontrun{
#' deimsid <- "https://deims.org/6b62feb2-61bf-47e1-b97f-0e909c408db8"
#' site_boundary <- ReLTER::get_site_info(deimsid = deimsid)
#' 
#' map <- create_leaflet_occ_map(
#'   occ_enriched = occ_eLTER_legal,
#'   site_boundary = site_boundary
#' )
#' map
#'
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
    "user.login", "uri", "taxon.default_photo.square_url"
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
  
  # Helper: extract nativeness from nested establishmentMeans tibble
  extract_nativeness <- function(em) {
    val <- tryCatch(em$nativeness[[1]], error = function(e) NA_character_)
    if (is.null(val) || length(val) == 0) NA_character_ else as.character(val)
  }
  
  # Helper: extract nativeness authority from nested establishmentMeans tibble
  extract_em_authority <- function(em) {
    val <- tryCatch(em$authority[[1]], error = function(e) NA_character_)
    if (is.null(val) || length(val) == 0) NA_character_ else as.character(val)
  }
  
  # Helper: build HTML block for IUCN status from nested status_IUCN tibble
  # Shows one row per geographic scope (global + regional)
  extract_iucn_html <- function(iucn_tbl) {
    if (is.null(iucn_tbl) || nrow(iucn_tbl) == 0) return("-")
    rows <- purrr::pmap_chr(iucn_tbl, function(status, authority, name, url) {
      scope <- if (is.na(name) || name == "") "Global" else name
      label <- iucn_labels[status]
      label <- if (is.na(label)) status else paste0(status, " – ", label)
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
      iconic       = dplyr::coalesce(taxon.iconic_taxon_name, "Unknown"),
      geopriv      = dplyr::coalesce(taxon_geoprivacy, "unknown"),
      # Unpack establishmentMeans list-column
      nativeness_lbl    = purrr::map_chr(establishmentMeans, extract_nativeness),
      em_authority_lbl  = purrr::map_chr(establishmentMeans, extract_em_authority),
      nativeness_lbl    = ifelse(is.na(nativeness_lbl), "-", nativeness_lbl),
      em_authority_lbl  = ifelse(is.na(em_authority_lbl), "-", em_authority_lbl),
      # Unpack status_IUCN list-column into an HTML string
      iucn_html    = purrr::map_chr(status_IUCN, extract_iucn_html),
      # Flat columns
      obs_quality_lbl   = ifelse(is.na(quality_grade), "-", as.character(quality_grade)),
      taxon_name_lbl    = ifelse(is.na(name), "-", as.character(name)),
      common_name_lbl   = ifelse(is.na(taxon.preferred_common_name), "-", as.character(taxon.preferred_common_name)),
      observed_on_lbl   = ifelse(is.na(observed_on), "-", as.character(observed_on)),
      posacc_lbl        = ifelse(is.na(public_positional_accuracy), "-", as.character(public_positional_accuracy)),
      observer_lbl      = ifelse(is.na(user.login), "-", as.character(user.login)),
      observer_url      = ifelse(is.na(user.login), "-", paste0("https://www.inaturalist.org/people/", user.login)),
      taxon_url         = paste0("https://www.inaturalist.org/taxa/", taxon.id),
      obs_url           = ifelse(is.na(uri), "", uri),
      photo_url         = ifelse(is.na(taxon.default_photo.square_url), "", taxon.default_photo.square_url)
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
      obs_quality    = occ_map$obs_quality_lbl,
      taxon_name     = occ_map$taxon_name_lbl,
      taxon_url      = occ_map$taxon_url,
      iucn_html      = occ_map$iucn_html,
      nativeness     = occ_map$nativeness_lbl,
      em_authority   = occ_map$em_authority_lbl,
      common_name    = occ_map$common_name_lbl,
      observed_on    = occ_map$observed_on_lbl,
      geopriv        = occ_map$geopriv,
      posacc         = occ_map$posacc_lbl,
      observer       = occ_map$observer_lbl,
      observer_url   = occ_map$observer_url,
      obs_url        = occ_map$obs_url,
      photo_url      = occ_map$photo_url,
      directive_html = occ_map$directive_html
    ),
    function(obs_quality, taxon_name, taxon_url, iucn_html, nativeness, em_authority,
             common_name, observed_on, geopriv, posacc,
             observer, observer_url, obs_url, photo_url, directive_html) {
      
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
        paste0("<b>EU Directives</b> - ", linked_html)
      } else ""
      
      paste0(
        # Photo
        if (nzchar(photo_url))
          sprintf(
            '<div style="margin-bottom:8px;text-align:center;">
               <img src="%s" style="width:110px;height:auto;border-radius:8px;">
             </div>', photo_url)
        else "",
        
        # Taxon info
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
          sprintf('<b>Observation:</b> <a href="%s" target="_blank">open record</a>', obs_url)
        else "",
        
        # Divider
        '<div style="height:1px;background:#e0e0e0;margin:8px 0;"></div>',
        
        # Data quality — geoprivacy with link
        "<b>Obs quality:</b> ", obs_quality, " <a href='https://help.inaturalist.org/en/support/solutions/articles/151000169936-what-is-the-data-quality-assessment-and-how-do-observations-qualify-to-become-research-grade-' target='_blank' style='font-size:11px;'>ℹ️</a><br/>",
        sprintf(
          '<b>Geoprivacy:</b> %s <a href="https://www.inaturalist.org/pages/geoprivacy" target="_blank" style="font-size:11px;">ℹ️</a><br/>',
          geopriv
        ),
        "<b>Positional accuracy (m):</b> ", posacc,
        
        # Divider
        '<div style="height:1px;background:#e0e0e0;margin:8px 0;"></div>',
        
        # IUCN status
        "<b>Status (IUCN):</b><br/>", iucn_html, "<br/>",
        
        # Establishment means — with link to iNaturalist help
        sprintf(
          '<b>Establishment means:</b> %s <a href="https://help.inaturalist.org/en/support/solutions/articles/151000176171-how-to-add-or-edit-establishment-means-in-inaturalist" target="_blank" style="font-size:11px;">ℹ️</a><br/>',
          nativeness
        ),
        "<b>Establishment means authority:</b> ", em_authority,
        
        # Divider
        '<div style="height:1px;background:#e0e0e0;margin:8px 0;"></div>',
        
        # EU Directives
        directive_block
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
      options       = leaflet::layersControlOptions(collapsed = FALSE)
    )
  return(map)
}

