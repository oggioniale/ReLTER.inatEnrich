# Tests for ReLTER.inatEnrich
# Framework: testthat
#
# File location: tests/testthat/test-ReLTER_inatEnrich.R
#
# Run all tests with:
#   devtools::test()
# or a single file with:
#   testthat::test_file("tests/testthat/test-ReLTER_inatEnrich.R")

library(testthat)

# ==============================================================================
# HELPERS — shared fixtures from package data
# ==============================================================================

occ <- ReLTER.inatEnrich::occ_eLTER_legal
sb  <- ReLTER.inatEnrich::site_boundary

# Mock site boundary for map tests
mock_site_boundary <- function() {
  sf::st_sf(
    title.x  = "Test site",
    uri      = "https://deims.org/6b62feb2-61bf-47e1-b97f-0e909c408db8",
    geometry = sf::st_sfc(sf::st_polygon(list(
      matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)
    )), crs = 4326)
  )
}

# ==============================================================================
# get_conservation_status()
# ==============================================================================

test_that("get_conservation_status() returns tibble with expected columns", {
  skip_on_ci()
  result <- get_conservation_status(taxon.id = occ$taxon.id[1])
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("status", "authority", "name", "url"))
})

test_that("get_conservation_status() returns correct column types", {
  skip_on_ci()
  result <- get_conservation_status(taxon.id = occ$taxon.id[1])
  expect_type(result$status,    "character")
  expect_type(result$authority, "character")
  expect_type(result$name,      "character")
  expect_type(result$url,       "character")
})

test_that("get_conservation_status() returns NA tibble for unknown taxon.id", {
  result <- get_conservation_status(taxon.id = 0)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("status", "authority", "name", "url"))
  expect_true(all(is.na(result$status)))
  expect_true(all(is.na(result$authority)))
})

test_that("get_conservation_status() status_IUCN in occ_eLTER_legal is well-formed", {
  purrr::walk(occ$status_IUCN, function(tbl) {
    expect_s3_class(tbl, "tbl_df")
    expect_named(tbl, c("status", "authority", "name", "url"))
  })
})

# ==============================================================================
# get_nativeness_degree()
# ==============================================================================

test_that("get_nativeness_degree() returns tibble with establishmentMeans", {
  skip_on_ci()
  result <- get_nativeness_degree(
    taxon.id = occ$taxon.id[1],
    country  = sb$country
  )
  expect_s3_class(result, "tbl_df")
  expect_named(result, "establishmentMeans")
  expect_type(result$establishmentMeans, "list")
})

test_that("get_nativeness_degree() nested tibble has nativeness and authority", {
  skip_on_ci()
  result <- get_nativeness_degree(
    taxon.id = occ$taxon.id[1],
    country  = sb$country
  )
  nested <- result$establishmentMeans[[1]]
  expect_s3_class(nested, "tbl_df")
  expect_named(nested, c("nativeness", "authority"))
})

test_that("get_nativeness_degree() returns NA when country is NULL", {
  expect_warning(
    result <- get_nativeness_degree(taxon.id = occ$taxon.id[1], country = NULL),
    "No country specified"
  )
  nested <- result$establishmentMeans[[1]]
  expect_true(is.na(nested$nativeness))
  expect_true(is.na(nested$authority))
})

test_that("get_nativeness_degree() returns NA for non-existent country", {
  skip_on_ci()
  result <- get_nativeness_degree(
    taxon.id = occ$taxon.id[1],
    country  = "NonExistentCountry"
  )
  nested <- result$establishmentMeans[[1]]
  expect_true(is.na(nested$nativeness))
})

test_that("establishmentMeans in occ_eLTER_legal is well-formed", {
  purrr::walk(occ$establishmentMeans, function(tbl) {
    expect_s3_class(tbl, "tbl_df")
    expect_named(tbl, c("nativeness", "authority"))
  })
})

# ==============================================================================
# get_eunis_legal_info()
# ==============================================================================

test_that("get_eunis_legal_info() returns tibble with expected columns", {
  skip_on_ci()
  taxon_with_directive <- occ |>
    sf::st_drop_geometry() |>
    dplyr::filter(!is.na(directive)) |>
    dplyr::slice(1)
  result <- get_eunis_legal_info(taxon.id = taxon_with_directive$taxon.id)
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("taxon.id", "scientific_name", "Legal text", "Annex") %in% names(result)))
})

test_that("get_eunis_legal_info() returns NA fallback for unknown taxon", {
  skip_on_ci()
  result <- get_eunis_legal_info(taxon.id = 0)
  expect_s3_class(result, "tbl_df")
  expect_true(is.na(result$`Legal text`[1]))
})

test_that("directive and annex in occ_eLTER_legal are character columns", {
  expect_type(occ$directive, "character")
  expect_type(occ$annex,     "character")
})

# ==============================================================================
# add_iucn_to_occ()
# ==============================================================================

test_that("add_iucn_to_occ() output has status_IUCN and has_IUCN columns", {
  expect_true("status_IUCN" %in% names(occ))
  expect_true("has_IUCN"    %in% names(occ))
})

test_that("add_iucn_to_occ() status_IUCN is a list-column of tibbles", {
  expect_type(occ$status_IUCN, "list")
  purrr::walk(occ$status_IUCN, ~ expect_s3_class(.x, "tbl_df"))
})

test_that("add_iucn_to_occ() each nested tibble has correct columns", {
  purrr::walk(occ$status_IUCN, function(tbl) {
    expect_named(tbl, c("status", "authority", "name", "url"))
  })
})

test_that("add_iucn_to_occ() has_IUCN is logical", {
  expect_type(occ$has_IUCN, "logical")
})

test_that("add_iucn_to_occ() only contains research-grade non-captive rows", {
  expect_true(all(occ$quality_grade == "research"))
  expect_true(all(occ$captive == FALSE))
})

# ==============================================================================
# add_nativeness_to_occ()
# ==============================================================================

test_that("add_nativeness_to_occ() output has establishmentMeans column", {
  expect_true("establishmentMeans" %in% names(occ))
})

test_that("add_nativeness_to_occ() establishmentMeans is a list-column of tibbles", {
  expect_type(occ$establishmentMeans, "list")
  purrr::walk(occ$establishmentMeans, ~ expect_s3_class(.x, "tbl_df"))
})

test_that("add_nativeness_to_occ() nested tibbles have nativeness and authority", {
  purrr::walk(occ$establishmentMeans, function(tbl) {
    expect_named(tbl, c("nativeness", "authority"))
  })
})

test_that("add_nativeness_to_occ() stops when country is NULL", {
  expect_error(
    add_nativeness_to_occ(occ_eLTER = occ, country = NULL),
    "`country` is required"
  )
})

# ==============================================================================
# add_eunis_legal_to_occ()
# ==============================================================================

test_that("add_eunis_legal_to_occ() output has directive and annex columns", {
  expect_true("directive" %in% names(occ))
  expect_true("annex"     %in% names(occ))
})

test_that("add_eunis_legal_to_occ() directive and annex are character", {
  expect_type(occ$directive, "character")
  expect_type(occ$annex,     "character")
})

test_that("add_eunis_legal_to_occ() stops when taxon.id column is missing", {
  occ_no_id <- occ |> dplyr::select(-taxon.id)
  expect_error(
    add_eunis_legal_to_occ(occ_eLTER = occ_no_id),
    "column named 'taxon.id'"
  )
})

# ==============================================================================
# create_leaflet_occ_map()
# ==============================================================================

test_that("create_leaflet_occ_map() returns a leaflet object", {
  result <- create_leaflet_occ_map(
    occ_enriched  = occ,
    site_boundary = sb
  )
  expect_s3_class(result, "leaflet")
})

test_that("create_leaflet_occ_map() works without site_boundary", {
  expect_no_error(
    create_leaflet_occ_map(occ_enriched = occ, site_boundary = NULL)
  )
})

test_that("create_leaflet_occ_map() stops when required columns are missing", {
  occ_missing <- occ |> dplyr::select(-status_IUCN)
  expect_error(
    create_leaflet_occ_map(occ_enriched = occ_missing),
    "required columns are missing"
  )
})

test_that("create_leaflet_occ_map() handles all-NA establishmentMeans", {
  occ_na_em <- occ
  occ_na_em$establishmentMeans <- lapply(
    seq_len(nrow(occ_na_em)),
    function(i) tibble::tibble(nativeness = NA_character_, authority = NA_character_)
  )
  expect_no_error(
    create_leaflet_occ_map(occ_enriched = occ_na_em, site_boundary = sb)
  )
})

test_that("create_leaflet_occ_map() handles all-NA status_IUCN", {
  occ_na_iucn <- occ
  occ_na_iucn$status_IUCN <- lapply(
    seq_len(nrow(occ_na_iucn)),
    function(i) tibble::tibble(
      status    = NA_character_,
      authority = NA_character_,
      name      = NA_character_,
      url       = NA_character_
    )
  )
  expect_no_error(
    create_leaflet_occ_map(occ_enriched = occ_na_iucn, site_boundary = sb)
  )
})

test_that("create_leaflet_occ_map() handles observations with no directives", {
  occ_no_dir <- occ
  occ_no_dir$directive <- rep(NA_character_, nrow(occ_no_dir))
  occ_no_dir$annex     <- rep(NA_character_, nrow(occ_no_dir))
  expect_no_error(
    create_leaflet_occ_map(occ_enriched = occ_no_dir, site_boundary = sb)
  )
})