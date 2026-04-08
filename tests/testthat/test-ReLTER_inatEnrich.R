# Tests for ReLTER.inatEnrich
# Framework : testthat
# API mocking: httptest2
#
# File location: tests/testthat/test-ReLTER_inatEnrich.R
#
# Run all tests with:
#   devtools::test()
# or a single file with:
#   testthat::test_file("tests/testthat/test-ReLTER_inatEnrich.R")

library(testthat)
library(httptest2)
library(dplyr)
library(tibble)

# ==============================================================================
# HELPERS — shared fixtures
# ==============================================================================

# Minimal valid iNaturalist API response for get_conservation_status()
# and get_nativeness_degree() — taxon.id 48484 (Quercus ilex)
mock_inat_taxa_response <- function() {
  list(
    results = list(
      list(
        id   = 48484,
        name = "Quercus ilex",
        conservation_statuses = list(
          tibble::tibble(
            status    = "LC",
            authority = "IUCN Red List",
            place     = list(list(name = NA_character_)),
            url       = "https://www.iucnredlist.org/species/example"
          )
        ),
        listed_taxa = list(
          list(
            establishment_means = "native",
            place = list(name = "Italy"),
            list  = list(title = "Italy Check List")
          )
        )
      )
    )
  )
}

# Minimal sf object mimicking occ_eLTER after the full enrichment pipeline.
# Leaflet requires an sf object with a geometry column so that
# addCircleMarkers() can infer coordinates correctly.
# --- Fixture A: plain tibble with list-columns ---
# Used for enrichment tests. sf::st_sf() does not support nested tibble
# list-columns, so we use a plain tibble here.
make_mock_occ <- function(n = 3) {
  tibble::tibble(
    taxon.id                       = c(48484L, 12345L, 99999L)[seq_len(n)],
    name                           = c("Quercus ilex", "Pinus pinea", "Canis lupus")[seq_len(n)],
    taxon.preferred_common_name    = c("Holm oak", "Stone pine", "Wolf")[seq_len(n)],
    taxon.iconic_taxon_name        = c("Plantae", "Plantae", "Mammalia")[seq_len(n)],
    taxon_geoprivacy               = c("open", "obscured", "open")[seq_len(n)],
    quality_grade                  = rep("research", n),
    observed_on                    = as.Date(c("2023-05-01", "2023-06-15", "2023-07-20")[seq_len(n)]),
    public_positional_accuracy     = c(10, 50, 5)[seq_len(n)],
    captive                        = rep(FALSE, n),
    user.login                     = c("user_a", "user_b", "user_c")[seq_len(n)],
    uri                            = paste0("https://www.inaturalist.org/observations/", c(1001, 1002, 1003)[seq_len(n)]),
    taxon.default_photo.square_url = paste0("https://static.inaturalist.org/photos/", seq_len(n), "/square.jpg"),
    status_IUCN = list(
      tibble::tibble(status = "LC", authority = "IUCN Red List",
                     name = NA_character_, url = "https://www.iucnredlist.org/species/example"),
      tibble::tibble(status = NA_character_, authority = NA_character_,
                     name = NA_character_, url = NA_character_),
      tibble::tibble(status = "EN", authority = "IUCN Red List",
                     name = "Europe", url = "https://www.iucnredlist.org/species/example2")
    )[seq_len(n)],
    establishmentMeans = list(
      tibble::tibble(nativeness = "native",     authority = "Italy Check List"),
      tibble::tibble(nativeness = NA_character_, authority = NA_character_),
      tibble::tibble(nativeness = "introduced", authority = "Italy Check List")
    )[seq_len(n)],
    directive = c("EU Habitats Directive (92/43/EEC)", NA, "EU Birds Directive (2009/147/EC)")[seq_len(n)],
    annex     = c("Annex IV", NA, "Annex I")[seq_len(n)],
    uri.1     = rep("https://deims.org/6b62feb2-61bf-47e1-b97f-0e909c408db8", n)
  )
}

# --- Fixture B: sf object with geometry ---
# Used only for create_leaflet_occ_map() tests.
# List-columns are injected after construction with [[<- 
# because sf::st_sf() does not support nested tibble list-columns directly.
make_mock_occ_sf <- function(n = 3) {
  lons <- c(10.741, 10.742, 10.743)[seq_len(n)]
  lats <- c(45.183, 45.184, 45.185)[seq_len(n)]
  
  obj <- sf::st_sf(
    taxon.id                       = c(48484L, 12345L, 99999L)[seq_len(n)],
    name                           = c("Quercus ilex", "Pinus pinea", "Canis lupus")[seq_len(n)],
    taxon.preferred_common_name    = c("Holm oak", "Stone pine", "Wolf")[seq_len(n)],
    taxon.iconic_taxon_name        = c("Plantae", "Plantae", "Mammalia")[seq_len(n)],
    taxon_geoprivacy               = c("open", "obscured", "open")[seq_len(n)],
    quality_grade                  = rep("research", n),
    observed_on                    = as.Date(c("2023-05-01", "2023-06-15", "2023-07-20")[seq_len(n)]),
    public_positional_accuracy     = c(10, 50, 5)[seq_len(n)],
    captive                        = rep(FALSE, n),
    user.login                     = c("user_a", "user_b", "user_c")[seq_len(n)],
    uri                            = paste0("https://www.inaturalist.org/observations/", c(1001, 1002, 1003)[seq_len(n)]),
    taxon.default_photo.square_url = paste0("https://static.inaturalist.org/photos/", seq_len(n), "/square.jpg"),
    directive = c("EU Habitats Directive (92/43/EEC)", NA_character_, "EU Birds Directive (2009/147/EC)")[seq_len(n)],
    annex     = c("Annex IV", NA_character_, "Annex I")[seq_len(n)],
    uri.1     = rep("https://deims.org/6b62feb2-61bf-47e1-b97f-0e909c408db8", n),
    geometry  = sf::st_sfc(
      lapply(seq_len(n), function(i) sf::st_point(c(lons[i], lats[i]))),
      crs = 4326
    )
  )
  
  # Inject nested tibble list-columns after sf construction
  obj[["status_IUCN"]] <- list(
    tibble::tibble(status = "LC", authority = "IUCN Red List",
                   name = NA_character_, url = "https://www.iucnredlist.org/species/example"),
    tibble::tibble(status = NA_character_, authority = NA_character_,
                   name = NA_character_, url = NA_character_),
    tibble::tibble(status = "EN", authority = "IUCN Red List",
                   name = "Europe", url = "https://www.iucnredlist.org/species/example2")
  )[seq_len(n)]
  
  obj[["establishmentMeans"]] <- list(
    tibble::tibble(nativeness = "native",     authority = "Italy Check List"),
    tibble::tibble(nativeness = NA_character_, authority = NA_character_),
    tibble::tibble(nativeness = "introduced", authority = "Italy Check List")
  )[seq_len(n)]
  
  obj
}

# ==============================================================================
# get_conservation_status()
# ==============================================================================

test_that("get_conservation_status() returns a tibble with expected columns", {
  with_mock_api({
    result <- get_conservation_status(taxon.id = 48484)
    expect_s3_class(result, "tbl_df")
    expect_named(result, c("status", "authority", "name", "url"))
  })
})

test_that("get_conservation_status() returns correct column types", {
  with_mock_api({
    result <- get_conservation_status(taxon.id = 48484)
    expect_type(result$status,    "character")
    expect_type(result$authority, "character")
    expect_type(result$name,      "character")
    expect_type(result$url,       "character")
  })
})

test_that("get_conservation_status() returns NA tibble for unknown taxon.id", {
  # Simulate 404 by using an ID that returns no results in mock
  with_mock_api({
    result <- get_conservation_status(taxon.id = 0)
    expect_s3_class(result, "tbl_df")
    expect_true(all(is.na(result$status)))
    expect_true(all(is.na(result$authority)))
  })
})

test_that("get_conservation_status() returns NA tibble when API is unreachable", {
  # Force network failure via httptest2 without any recorded mock
  without_internet({
    result <- get_conservation_status(taxon.id = 48484)
    expect_s3_class(result, "tbl_df")
    expect_true(all(is.na(result$status)))
  })
})

# ==============================================================================
# get_nativeness_degree()
# ==============================================================================

test_that("get_nativeness_degree() returns a tibble with establishmentMeans list-column", {
  with_mock_api({
    result <- get_nativeness_degree(taxon.id = 48484, country = "Italy")
    expect_s3_class(result, "tbl_df")
    expect_named(result, "establishmentMeans")
    expect_type(result$establishmentMeans, "list")
  })
})

test_that("get_nativeness_degree() nested tibble has nativeness and authority columns", {
  with_mock_api({
    result  <- get_nativeness_degree(taxon.id = 48484, country = "Italy")
    nested  <- result$establishmentMeans[[1]]
    expect_s3_class(nested, "tbl_df")
    expect_named(nested, c("nativeness", "authority"))
    expect_type(nested$nativeness, "character")
    expect_type(nested$authority,  "character")
  })
})

test_that("get_nativeness_degree() returns NA when country is NULL", {
  expect_warning(
    result <- get_nativeness_degree(taxon.id = 48484, country = NULL),
    "No country specified"
  )
  nested <- result$establishmentMeans[[1]]
  expect_true(is.na(nested$nativeness))
  expect_true(is.na(nested$authority))
})

test_that("get_nativeness_degree() returns NA when country does not match", {
  with_mock_api({
    result <- get_nativeness_degree(taxon.id = 48484, country = "NonExistentCountry")
    nested <- result$establishmentMeans[[1]]
    expect_true(is.na(nested$nativeness))
  })
})

test_that("get_nativeness_degree() returns NA tibble when API is unreachable", {
  without_internet({
    result <- get_nativeness_degree(taxon.id = 48484, country = "Italy")
    nested <- result$establishmentMeans[[1]]
    expect_true(is.na(nested$nativeness))
  })
})

# ==============================================================================
# get_eunis_legal_info()
# Note: this function uses httr + rvest (not httr2), so httptest2 cannot
# capture its HTTP calls. All tests use local_mocked_bindings() (Option A)
# to substitute the function with a controlled fixture.
# ==============================================================================

# Shared fixture: valid EUNIS response for taxon.id 61749 (Falco peregrinus)
eunis_mock_valid <- function(taxon.id) {
  tibble::tibble(
    taxon.id        = as.integer(taxon.id),
    scientific_name = "Falco peregrinus",
    `Legal text`    = c(
      "EU Birds Directive (2009/147/EC)",
      "EU Habitats Directive (92/43/EEC)"
    ),
    Annex = c("Annex I", "Annex IV")
  )
}

# Shared fixture: safe fallback (no legal info found)
eunis_mock_empty <- function(taxon.id) {
  tibble::tibble(
    taxon.id        = as.integer(taxon.id),
    scientific_name = NA_character_,
    `Legal text`    = NA_character_,
    Annex           = NA_character_
  )
}

test_that("get_eunis_legal_info() returns a tibble with expected columns", {
  local_mocked_bindings(
    get_eunis_legal_info = eunis_mock_valid,
    .package = "ReLTER.inatEnrich"
  )
  result <- get_eunis_legal_info(taxon.id = 61749)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("taxon.id", "scientific_name", "Legal text", "Annex") %in% names(result)))
})

test_that("get_eunis_legal_info() returns correct column types", {
  local_mocked_bindings(
    get_eunis_legal_info = eunis_mock_valid,
    .package = "ReLTER.inatEnrich"
  )
  result <- get_eunis_legal_info(taxon.id = 61749)
  expect_type(result$taxon.id,        "integer")
  expect_type(result$scientific_name, "character")
  expect_type(result$`Legal text`,    "character")
  expect_type(result$Annex,           "character")
})

test_that("get_eunis_legal_info() can return multiple rows (one per directive)", {
  local_mocked_bindings(
    get_eunis_legal_info = eunis_mock_valid,
    .package = "ReLTER.inatEnrich"
  )
  result <- get_eunis_legal_info(taxon.id = 61749)
  expect_gte(nrow(result), 1L)
})

test_that("get_eunis_legal_info() returns safe fallback for unknown taxon", {
  local_mocked_bindings(
    get_eunis_legal_info = eunis_mock_empty,
    .package = "ReLTER.inatEnrich"
  )
  result <- get_eunis_legal_info(taxon.id = 0)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1L)
  expect_true(is.na(result$`Legal text`))
  expect_true(is.na(result$Annex))
})

test_that("get_eunis_legal_info() returns safe fallback when API is unreachable", {
  local_mocked_bindings(
    get_eunis_legal_info = eunis_mock_empty,
    .package = "ReLTER.inatEnrich"
  )
  result <- get_eunis_legal_info(taxon.id = 61749)
  expect_s3_class(result, "tbl_df")
  expect_true(is.na(result$`Legal text`))
})

# ==============================================================================
# add_iucn_to_occ()
# ==============================================================================

test_that("add_iucn_to_occ() returns a tibble with status_IUCN and has_IUCN columns", {
  occ <- make_mock_occ() |>
    dplyr::select(-status_IUCN, -establishmentMeans, -directive, -annex)
  
  with_mock_api({
    result <- add_iucn_to_occ(occ_eLTER = occ)
    expect_s3_class(result, "tbl_df")
    expect_true("status_IUCN" %in% names(result))
    expect_true("has_IUCN"    %in% names(result))
  })
})

test_that("add_iucn_to_occ() status_IUCN is a list-column of tibbles", {
  occ <- make_mock_occ() |>
    dplyr::select(-status_IUCN, -establishmentMeans, -directive, -annex)
  
  with_mock_api({
    result <- add_iucn_to_occ(occ_eLTER = occ)
    expect_type(result$status_IUCN, "list")
    purrr::walk(result$status_IUCN, ~ expect_s3_class(.x, "tbl_df"))
  })
})

test_that("add_iucn_to_occ() each nested tibble has correct columns", {
  occ <- make_mock_occ() |>
    dplyr::select(-status_IUCN, -establishmentMeans, -directive, -annex)
  
  with_mock_api({
    result <- add_iucn_to_occ(occ_eLTER = occ)
    purrr::walk(result$status_IUCN, function(tbl) {
      expect_named(tbl, c("status", "authority", "name", "url"))
    })
  })
})

test_that("add_iucn_to_occ() filters only research-grade non-captive rows", {
  occ <- make_mock_occ(n = 3) |>
    dplyr::select(-status_IUCN, -establishmentMeans, -directive, -annex) |>
    dplyr::mutate(
      quality_grade = c("research", "casual", "research"),
      captive       = c(FALSE, FALSE, TRUE)
    )
  
  with_mock_api({
    result <- add_iucn_to_occ(occ_eLTER = occ)
    # Only 1 row passes both filters
    expect_equal(nrow(result), 1L)
  })
})

# ==============================================================================
# add_nativeness_to_occ()
# ==============================================================================

test_that("add_nativeness_to_occ() returns a tibble with establishmentMeans column", {
  occ <- make_mock_occ() |>
    dplyr::select(-establishmentMeans, -directive, -annex)
  
  with_mock_api({
    result <- add_nativeness_to_occ(occ_eLTER = occ, country = "Italy")
    expect_s3_class(result, "tbl_df")
    expect_true("establishmentMeans" %in% names(result))
  })
})

test_that("add_nativeness_to_occ() establishmentMeans is a list-column of tibbles", {
  occ <- make_mock_occ() |>
    dplyr::select(-establishmentMeans, -directive, -annex)
  
  with_mock_api({
    result <- add_nativeness_to_occ(occ_eLTER = occ, country = "Italy")
    expect_type(result$establishmentMeans, "list")
    purrr::walk(result$establishmentMeans, ~ expect_s3_class(.x, "tbl_df"))
  })
})

test_that("add_nativeness_to_occ() nested tibbles have nativeness and authority", {
  occ <- make_mock_occ() |>
    dplyr::select(-establishmentMeans, -directive, -annex)
  
  with_mock_api({
    result <- add_nativeness_to_occ(occ_eLTER = occ, country = "Italy")
    purrr::walk(result$establishmentMeans, function(tbl) {
      expect_named(tbl, c("nativeness", "authority"))
    })
  })
})

test_that("add_nativeness_to_occ() stops when country is NULL", {
  occ <- make_mock_occ() |>
    dplyr::select(-establishmentMeans, -directive, -annex)
  expect_error(
    add_nativeness_to_occ(occ_eLTER = occ, country = NULL),
    "`country` is required"
  )
})

# ==============================================================================
# add_eunis_legal_to_occ()
# Note: get_eunis_legal_info() is mocked via local_mocked_bindings()
# throughout this section to avoid live HTTP calls.
# ==============================================================================

test_that("add_eunis_legal_to_occ() returns a tibble with directive and annex columns", {
  occ <- make_mock_occ() |>
    dplyr::select(-directive, -annex)
  
  local_mocked_bindings(
    get_eunis_legal_info = eunis_mock_valid,
    .package = "ReLTER.inatEnrich"
  )
  result <- add_eunis_legal_to_occ(occ_eLTER = occ)
  expect_s3_class(result, "tbl_df")
  expect_true("directive" %in% names(result))
  expect_true("annex"     %in% names(result))
})

test_that("add_eunis_legal_to_occ() renames Legal text -> directive and Annex -> annex", {
  occ <- make_mock_occ() |>
    dplyr::select(-directive, -annex)
  
  local_mocked_bindings(
    get_eunis_legal_info = eunis_mock_valid,
    .package = "ReLTER.inatEnrich"
  )
  result <- add_eunis_legal_to_occ(occ_eLTER = occ)
  # Original column names from get_eunis_legal_info() must not appear
  expect_false("Legal text" %in% names(result))
  expect_false("Annex"      %in% names(result))
})

test_that("add_eunis_legal_to_occ() stops when taxon.id column is missing", {
  occ <- make_mock_occ() |>
    dplyr::select(-taxon.id, -directive, -annex)
  expect_error(
    add_eunis_legal_to_occ(occ_eLTER = occ),
    "column named 'taxon.id'"
  )
})

test_that("add_eunis_legal_to_occ() returns NA directive for taxa not in EUNIS", {
  occ <- tibble::tibble(
    taxon.id      = 0L,
    name          = "Unknown species",
    quality_grade = "research",
    observed_on   = as.Date("2023-01-01"),
    captive       = FALSE
  )
  
  local_mocked_bindings(
    get_eunis_legal_info = eunis_mock_empty,
    .package = "ReLTER.inatEnrich"
  )
  result <- add_eunis_legal_to_occ(occ_eLTER = occ)
  expect_true(is.na(result$directive[[1]]))
  expect_true(is.na(result$annex[[1]]))
})

# ==============================================================================
# create_leaflet_occ_map()
# ==============================================================================

test_that("create_leaflet_occ_map() returns a leaflet object", {
  occ <- make_mock_occ_sf()
  
  # Mock get_site_info() to avoid external call
  local_mocked_bindings(
    get_site_info = function(...) {
      sf::st_sf(
        title.x = "Test site",
        uri     = "https://deims.org/6b62feb2-61bf-47e1-b97f-0e909c408db8",
        geometry = sf::st_sfc(sf::st_polygon(list(
          matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE)
        )), crs = 4326)
      )
    },
    .package = "ReLTER"
  )
  
  result <- create_leaflet_occ_map(occ_enriched = occ)
  expect_s3_class(result, "leaflet")
})

test_that("create_leaflet_occ_map() stops when required columns are missing", {
  occ <- make_mock_occ_sf() |>
    dplyr::select(-status_IUCN)   # remove a required column
  expect_error(
    create_leaflet_occ_map(occ_enriched = occ),
    "required columns are missing"
  )
})

test_that("create_leaflet_occ_map() handles all-NA establishmentMeans without error", {
  occ <- make_mock_occ_sf()
  occ$establishmentMeans <- lapply(
    seq_len(nrow(occ)),
    function(i) tibble::tibble(nativeness = NA_character_, authority = NA_character_)
  )
  
  local_mocked_bindings(
    get_site_info = function(...) {
      sf::st_sf(
        title.x = "Test site",
        uri     = "https://deims.org/test",
        geometry = sf::st_sfc(sf::st_polygon(list(
          matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE)
        )), crs = 4326)
      )
    },
    .package = "ReLTER"
  )
  
  expect_no_error(create_leaflet_occ_map(occ_enriched = occ))
})

test_that("create_leaflet_occ_map() handles all-NA status_IUCN without error", {
  occ <- make_mock_occ_sf()
  occ$status_IUCN <- lapply(
    seq_len(nrow(occ)),
    function(i) tibble::tibble(
      status    = NA_character_,
      authority = NA_character_,
      name      = NA_character_,
      url       = NA_character_
    )
  )
  
  local_mocked_bindings(
    get_site_info = function(...) {
      sf::st_sf(
        title.x = "Test site",
        uri     = "https://deims.org/test",
        geometry = sf::st_sfc(sf::st_polygon(list(
          matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE)
        )), crs = 4326)
      )
    },
    .package = "ReLTER"
  )
  
  expect_no_error(create_leaflet_occ_map(occ_enriched = occ))
})

test_that("create_leaflet_occ_map() handles observations with no directives", {
  occ <- make_mock_occ_sf()
  occ$directive <- rep(NA_character_, nrow(occ))
  occ$annex     <- rep(NA_character_, nrow(occ))
  
  local_mocked_bindings(
    get_site_info = function(...) {
      sf::st_sf(
        title.x = "Test site",
        uri     = "https://deims.org/test",
        geometry = sf::st_sfc(sf::st_polygon(list(
          matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE)
        )), crs = 4326)
      )
    },
    .package = "ReLTER"
  )
  
  expect_no_error(create_leaflet_occ_map(occ_enriched = occ))
})

