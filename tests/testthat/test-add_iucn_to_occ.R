test_that("add_iucn_to_occ returns expected structure", {
  
  # Fake minimal input
  occ <- data.frame(
    taxon.id = c(1, 2),
    latitude = c(45, 46),
    longitude = c(9, 10)
  )
  
  # Convert to sf (minimal geometry)
  occ <- sf::st_as_sf(
    occ,
    coords = c("longitude", "latitude"),
    crs = 4326
  )
  
  # Mock function (if your function internally calls API, you should mock it)
  # Here we assume the function can run without failing
  
  result <- add_iucn_to_occ(occ)
  
  # --- Basic checks ---
  expect_s3_class(result, "sf")
  expect_true("status_IUCN" %in% names(result))
  expect_true("has_iucn_status" %in% names(result))
  
  # --- Column types ---
  expect_true(is.list(result$status_IUCN))
  expect_true(is.logical(result$has_iucn_status))
  
  # --- Structure of nested tibble ---
  first_entry <- result$status_IUCN[[1]]
  
  if (!is.null(first_entry)) {
    expect_true(all(c("status", "authority", "scope", "url") %in% names(first_entry)))
  }
  
})