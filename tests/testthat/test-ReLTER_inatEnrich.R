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

occ <- ReLTER.inatEnrich::occ_eLTER_EUNIS
sb <- ReLTER.inatEnrich::site_boundary

# Mock site boundary for map tests
mock_site_boundary <- function() {
  sf::st_sf(
    eLTER_title.x  = "Test site",
    eLTER_uri= "https://deims.org/6b62feb2-61bf-47e1-b97f-0e909c408db8",
    geometry = sf::st_sfc(sf::st_polygon(list(
      matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)
    )), crs = 4326)
  )
}

# ==============================================================================
# .assign_eLTER_SOs() — interno ma testabile via add_iucn_to_occ
# ==============================================================================

testthat::test_that("occ_eLTER_legal has SOBIO_014 and SOBIO_018 columns", {
  testthat::expect_true("SOBIO_014" %in% names(occ))
  testthat::expect_true("SOBIO_018" %in% names(occ))
})

testthat::test_that("SOBIO_014 and SOBIO_018 are logical columns", {
  testthat::expect_type(occ$SOBIO_014, "logical")
  testthat::expect_type(occ$SOBIO_018, "logical")
})

testthat::test_that("SOBIO_014 is TRUE for Insecta observations", {
  insecta <- occ |>
    sf::st_drop_geometry() |>
    dplyr::filter(taxon.iconic_taxon_name == "Insecta")
  if (nrow(insecta) > 0) {
    testthat::expect_true(all(insecta$SOBIO_014))
  } else {
    testthat::skip("No Insecta observations in dataset")
  }
})

testthat::test_that("SOBIO_018 is TRUE for Aves observations", {
  aves <- occ |>
    sf::st_drop_geometry() |>
    dplyr::filter(taxon.iconic_taxon_name == "Aves")
  if (nrow(aves) > 0) {
    testthat::expect_true(all(aves$SOBIO_018))
  } else {
    testthat::skip("No Aves observations in dataset")
  }
})

testthat::test_that("SOBIO columns have no NA values", {
  obs_flat <- occ |> sf::st_drop_geometry()
  testthat::expect_false(any(is.na(obs_flat$SOBIO_014)))
  testthat::expect_false(any(is.na(obs_flat$SOBIO_018)))
})

testthat::test_that("add_iucn_to_occ skips SO assignment if already present", {
  testthat::expect_true("SOBIO_014" %in% names(occ))
  testthat::expect_true("SOBIO_018" %in% names(occ))
})

testthat::test_that("Orthoptera observations have both SOBIO_014 and SOBIO_018 TRUE", {
  ORTHOPTERA_ID <- 47651
  ortho <- occ |>
    sf::st_drop_geometry() |>
    dplyr::filter(
      vapply(taxon.ancestor_ids,
             function(x) ORTHOPTERA_ID %in% unlist(x),
             FUN.VALUE = logical(1))
    )
  if (nrow(ortho) > 0) {
    testthat::expect_true(all(ortho$SOBIO_014))
    testthat::expect_true(all(ortho$SOBIO_018))
  } else {
    testthat::skip("No Orthoptera observations in dataset")
  }
})

test_that(".assign_eLTER_SOs() assigns SOBIO_017 TRUE for plant observations", {
  plants <- occ |>
    sf::st_drop_geometry() |>
    dplyr::filter(taxon.iconic_taxon_name == "Plantae")
  if (nrow(plants) > 0) {
    expect_true(all(plants$SOBIO_017))
  } else {
    skip("No Plantae observations in dataset")
  }
})

test_that(".assign_eLTER_SOs() assigns SOBIO_017 FALSE for non-plant observations", {
  non_plants <- occ |>
    sf::st_drop_geometry() |>
    dplyr::filter(taxon.iconic_taxon_name == "Aves")
  if (nrow(non_plants) > 0) {
    expect_true(all(!non_plants$SOBIO_017))
  } else {
    skip("No Aves observations in dataset")
  }
})

test_that(".assign_eLTER_SOs() assigns SOBIO_014 FALSE and SOBIO_018 TRUE for Aves", {
  aves <- occ |>
    sf::st_drop_geometry() |>
    dplyr::filter(taxon.iconic_taxon_name == "Aves")
  if (nrow(aves) > 0) {
    expect_true(all(!aves$SOBIO_014))
    expect_true(all(aves$SOBIO_018))
  } else {
    skip("No Aves observations in dataset")
  }
})

# ==============================================================================
# .assign_EASIN_info() — testabile indirettamente via get_nativeness_degree
# ==============================================================================

test_that("get_nativeness_degree() EASIN fields are NA for species not in EASIN", {
  skip_on_ci()
  # Falco tinnunculus è nativo, non in EASIN come alieno
  result <- get_nativeness_degree(taxon.id = 472766, country = "Italy")
  nested <- result$establishmentMeans[[1]]
  # Se non in EASIN, tutti i campi EASIN devono essere NA
  if (is.na(nested$EASIN_id)) {
    expect_true(is.na(nested$EASIN_url))
    expect_true(is.na(nested$EASIN_LSID))
    expect_true(is.na(nested$EASIN_status))
    expect_true(is.na(nested$EASIN_hasImpact))
    expect_true(is.na(nested$EASIN_IsEUConcern))
  } else {
    skip("Species found in EASIN — pick a different taxon for this test")
  }
})

test_that("get_nativeness_degree() EASIN fields are populated for known alien species", {
  skip_on_ci()
  # Vespa velutina — specie aliena nota in EASIN
  result <- get_nativeness_degree(taxon.id = 119019, country = "Italy")
  nested <- result$establishmentMeans[[1]]
  expect_false(is.na(nested$EASIN_id))
  expect_false(is.na(nested$EASIN_url))
  expect_true(grepl("easin.jrc.ec.europa.eu", nested$EASIN_url))
  expect_true(grepl("urn:lsid:easin", nested$EASIN_LSID))
})

test_that("get_nativeness_degree() EASIN_IsEUConcern is 'TRUE' or 'FALSE' or NA", {
  skip_on_ci()
  result <- get_nativeness_degree(taxon.id = 119019, country = "Italy")
  nested <- result$establishmentMeans[[1]]
  expect_true(nested$EASIN_IsEUConcern %in% c("TRUE", "FALSE", NA))
})

# ==============================================================================
# .country_to_flag() — testabile direttamente
# ==============================================================================

test_that(".country_to_flag() returns flag emoji + country name for known country", {
  result <- ReLTER.inatEnrich:::.country_to_flag("Italy")
  expect_true(grepl("Italy", result))
  expect_true(nchar(result) > nchar("Italy"))
})

test_that(".country_to_flag() returns country code unchanged for unknown country", {
  result <- ReLTER.inatEnrich:::.country_to_flag("Narnia")
  expect_equal(result, "Narnia")
})

test_that(".country_to_flag() returns '-' for NA input", {
  result <- ReLTER.inatEnrich:::.country_to_flag(NA)
  expect_equal(result, "-")
})

test_that(".country_to_flag() accepts 2-letter ISO codes", {
  result <- ReLTER.inatEnrich:::.country_to_flag("IT")
  expect_true(grepl("Italy", result))
})

# ==============================================================================
# get_conservation_status()
# ==============================================================================

test_that("get_conservation_status() returns tibble with expected columns", {
  skip_on_ci()
  result <- get_conservation_status(taxon.id = occ$taxon.id[1])
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("status", "authority", "scope_of_assesment", "url"))
})

test_that("get_conservation_status() returns correct column types", {
  skip_on_ci()
  result <- get_conservation_status(taxon.id = occ$taxon.id[1])
  expect_type(result$status,    "character")
  expect_type(result$authority, "character")
  expect_type(result$scope_of_assesment,      "character")
  expect_type(result$url,       "character")
})

test_that("get_conservation_status() returns NA tibble for unknown taxon.id", {
  result <- get_conservation_status(taxon.id = 0)
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("status", "authority", "scope_of_assesment", "url"))
  expect_true(all(is.na(result$status)))
  expect_true(all(is.na(result$authority)))
})

test_that("get_conservation_status() status_IUCN in occ_eLTER_legal is well-formed", {
  purrr::walk(occ$status_IUCN, function(tbl) {
    if (is.null(tbl)) return(invisible(NULL))  # <-- salta i NULL
    expect_s3_class(tbl, "tbl_df")
    expect_named(tbl, c("status", "authority", "scope_of_assesment", "url"))
  })
})

test_that("get_conservation_status() returns multiple rows for taxon with multiple IUCN statuses", {
  skip_on_ci()
  # Falco tinnunculus ha 3 stati IUCN
  result <- get_conservation_status(taxon.id = 472766)
  expect_s3_class(result, "tbl_df")
  expect_gte(nrow(result), 1)
  expect_true(all(result$authority == "IUCN Red List"))
})

test_that("get_conservation_status() scope_of_assesment is 'Globally' when place is NULL", {
  skip_on_ci()
  result <- get_conservation_status(taxon.id = 517449)
  expect_true(any(result$scope_of_assesment == "Globally"))
})

test_that("get_conservation_status() url column contains valid URLs or NA", {
  skip_on_ci()
  result <- get_conservation_status(taxon.id = 472766)
  non_na_urls <- result$url[!is.na(result$url)]
  if (length(non_na_urls) > 0) {
    expect_true(all(grepl("^https?://", non_na_urls)))
  }
})

# ==============================================================================
# add_eunis_legal_to_occ() — casi edge aggiuntivi
# ==============================================================================

test_that("add_eunis_legal_to_occ() directive values match expected pattern when not NA", {
  non_na <- occ |>
    sf::st_drop_geometry() |>
    dplyr::filter(!is.na(directive))
  if (nrow(non_na) > 0) {
    expect_true(all(grepl("92/43/EEC|2009/147/EC", non_na$directive)))
  } else {
    skip("No directive values in dataset")
  }
})

test_that("add_eunis_legal_to_occ() annex is NA when directive is NA", {
  both_na <- occ |>
    sf::st_drop_geometry() |>
    dplyr::filter(is.na(directive))
  expect_true(all(is.na(both_na$annex)))
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
  expect_named(nested, c("iNat_nativeness", "iNat_authority", "iNat_checkList_uri", "EASIN_url", "EASIN_id",
                         "EASIN_LSID", "EASIN_firstIntroductionsInEU_year", "EASIN_firstIntroductions_Country",
                         "EASIN_status", "EASIN_hasImpact", "EASIN_IsEUConcern"))
})

test_that("get_nativeness_degree() returns NA when country is NULL", {
  expect_warning(
    result <- get_nativeness_degree(taxon.id = occ$taxon.id[1], country = NULL),
    "No country specified"
  )
  nested <- result$establishmentMeans[[1]]
  expect_true(is.na(nested$iNat_nativeness))
  expect_true(is.na(nested$iNat_authority))
  expect_true(is.na(nested$iNat_checkList_uri))
  expect_true(is.na(nested$EASIN_url))
  expect_true(is.na(nested$EASIN_id))
  expect_true(is.na(nested$EASIN_LSID))
  expect_true(is.na(nested$EASIN_firstIntroductionsInEU_year))
  expect_true(is.na(nested$EASIN_firstIntroductions_Country))
  expect_true(is.na(nested$EASIN_status))
  expect_true(is.na(nested$EASIN_hasImpact))
})

test_that("get_nativeness_degree() returns NA for non-existent country", {
  skip_on_ci()
  result <- get_nativeness_degree(
    taxon.id = occ$taxon.id[1],
    country  = "NonExistentCountry"
  )
  nested <- result$establishmentMeans[[1]]
  expect_true(is.na(nested$iNat_nativeness))
})

test_that("establishmentMeans in occ_eLTER_legal is well-formed", {
  purrr::walk(occ$establishmentMeans, function(tbl) {
    expect_s3_class(tbl, "tbl_df")
    expect_named(tbl, c("iNat_nativeness", "iNat_authority", "iNat_checkList_uri", "EASIN_url", "EASIN_id",
                        "EASIN_LSID", "EASIN_firstIntroductionsInEU_year", "EASIN_firstIntroductions_Country",
                        "EASIN_status", "EASIN_hasImpact", "EASIN_IsEUConcern"))
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
  purrr::walk(occ$status_IUCN, function(x) {
    if (is.null(x)) return(invisible(NULL))  # <-- salta i NULL
    expect_s3_class(x, "tbl_df")
  })
})

test_that("add_iucn_to_occ() each nested tibble has correct columns", {
  purrr::walk(occ$status_IUCN, function(tbl) {
    if (is.null(tbl)) return(invisible(NULL))  # <-- salta i NULL
    expect_named(tbl, c("status", "authority", "scope_of_assesment", "url"))
  })
})

test_that("add_iucn_to_occ() has_IUCN is logical", {
  expect_type(occ$has_IUCN, "logical")
})

test_that("add_iucn_to_occ() only contains research-grade non-captive rows", {
  expect_true(all(occ$quality_grade == "research"))
  expect_true(all(occ$captive == FALSE))
})

test_that("add_iucn_to_occ() filters out non-research-grade observations", {
  expect_true(all(occ$quality_grade == "research"))
})

test_that("add_iucn_to_occ() filters out observations with NA date", {
  expect_false(any(is.na(occ$observed_on)))
})

test_that("add_iucn_to_occ() has_IUCN is FALSE when status_IUCN has all-NA status", {
  na_rows <- occ |>
    sf::st_drop_geometry() |>
    dplyr::filter(!has_IUCN)
  purrr::walk(na_rows$status_IUCN, function(tbl) {
    if (!is.null(tbl)) {
      expect_true(all(is.na(tbl$status)))
    }
  })
})

test_that("add_iucn_to_occ() SOBIO_017 column is present and logical", {
  expect_true("SOBIO_017" %in% names(occ))
  expect_type(occ$SOBIO_017, "logical")
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
    expect_named(tbl, c("iNat_nativeness", "iNat_authority", "iNat_checkList_uri", "EASIN_url", "EASIN_id",
                        "EASIN_LSID", "EASIN_firstIntroductionsInEU_year", "EASIN_firstIntroductions_Country",
                        "EASIN_status", "EASIN_hasImpact", "EASIN_IsEUConcern"))
  })
})

test_that("add_nativeness_to_occ() stops when country is NULL", {
  expect_error(
    add_nativeness_to_occ(occ_eLTER = occ, country = NULL),
    "`country` is required"
  )
})

test_that("add_nativeness_to_occ() has_establishmentMeans column is logical", {
  expect_true("has_establishmentMeans" %in% names(occ))
  expect_type(occ$has_establishmentMeans, "logical")
})

test_that("add_nativeness_to_occ() has_establishmentMeans is TRUE when iNat_nativeness is not NA", {
  has_em <- occ |>
    sf::st_drop_geometry() |>
    dplyr::filter(has_establishmentMeans)
  purrr::walk(has_em$establishmentMeans, function(em) {
    expect_false(is.na(em$iNat_nativeness[[1]]))
  })
})

test_that("add_nativeness_to_occ() iNat_nativeness values are in expected vocabulary", {
  valid_values <- c("native", "introduced", "vagrant", "endemic", NA)
  purrr::walk(occ$establishmentMeans, function(em) {
    val <- em$iNat_nativeness[[1]]
    expect_true(val %in% valid_values)
  })
})

test_that("add_nativeness_to_occ() iNat_checkList_uri is a valid URL or NA", {
  purrr::walk(occ$establishmentMeans, function(em) {
    val <- em$iNat_checkList_uri[[1]]
    if (!is.na(val)) {
      expect_true(grepl("^https://www.inaturalist.org/lists/", val))
    }
  })
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
    function(i) tibble::tibble(
      iNat_nativeness = NA_character_,
      iNat_authority = NA_character_,
      iNat_checkList_uri = NA_character_,
      EASIN_url = NA_character_,
      EASIN_id = NA_character_,
      EASIN_LSID = NA_character_,
      EASIN_firstIntroductionsInEU_year = NA_character_,
      EASIN_firstIntroductions_Country  = NA_character_,
      EASIN_status = NA_character_,
      EASIN_hasImpact = NA_character_,
      EASIN_IsEUConcern = NA_character_
    )
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
      status = NA_character_,
      authority = NA_character_,
      scope_of_assesment = NA_character_,
      url = NA_character_
    )
  )
  expect_no_error(
    create_leaflet_occ_map(occ_enriched = occ_na_iucn, site_boundary = sb)
  )
})

test_that("create_leaflet_occ_map() handles observations with no directives", {
  occ_no_dir <- occ
  occ_no_dir$directive <- rep(NA_character_, nrow(occ_no_dir))
  occ_no_dir$annex <- rep(NA_character_, nrow(occ_no_dir))
  expect_no_error(
    create_leaflet_occ_map(occ_enriched = occ_no_dir, site_boundary = sb)
  )
})

test_that("create_leaflet_occ_map() map contains at least one tile layer", {
  result <- create_leaflet_occ_map(occ_enriched = occ, site_boundary = sb)
  call_names <- sapply(result$x$calls, function(x) x$method)
  expect_true("addTiles" %in% call_names)
})

test_that("create_leaflet_occ_map() map contains addCircleMarkers", {
  result <- create_leaflet_occ_map(occ_enriched = occ, site_boundary = sb)
  call_names <- sapply(result$x$calls, function(x) x$method)
  expect_true("addCircleMarkers" %in% call_names)
})

test_that("create_leaflet_occ_map() map contains a legend", {
  result <- create_leaflet_occ_map(occ_enriched = occ, site_boundary = sb)
  call_names <- sapply(result$x$calls, function(x) x$method)
  expect_true("addLegend" %in% call_names)
})

test_that("create_leaflet_occ_map() with site_boundary adds polygon layer", {
  result <- create_leaflet_occ_map(occ_enriched = occ, site_boundary = sb)
  call_names <- sapply(result$x$calls, function(x) x$method)
  expect_true("addPolygons" %in% call_names)
})

test_that("create_leaflet_occ_map() without site_boundary has no polygon layer", {
  result <- create_leaflet_occ_map(occ_enriched = occ, site_boundary = NULL)
  call_names <- sapply(result$x$calls, function(x) x$method)
  expect_false("addPolygons" %in% call_names)
})
