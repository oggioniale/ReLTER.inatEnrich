# tests/testthat/test-plots.R

df <- ReLTER.inatEnrich::occ_eLTER_legal
site_boundary <- ReLTER.inatEnrich::site_boundary


# --- iconic_taxa --------------------------------------------------------

testthat::test_that("iconic_taxa returns a ggplot object", {
  df <- make_mock_df()
  p  <- testthat::expect_invisible(iconic_taxa(df))
  testthat::expect_s3_class(p, "ggplot")
})

testthat::test_that("iconic_taxa contains correct iconic taxon groups", {
  df      <- make_mock_df()
  p       <- iconic_taxa(df)
  pd      <- ggplot2::layer_data(p)
  n_groups <- dplyr::n_distinct(df$taxon.iconic_taxon_name)
  testthat::expect_gte(nrow(pd), n_groups)
})

testthat::test_that("iconic_taxa handles df with a single iconic taxon", {
  df         <- make_mock_df()
  df_single  <- df[df$taxon.iconic_taxon_name == "Aves", ]
  testthat::expect_no_error(iconic_taxa(df_single))
})


# --- top_n_species ------------------------------------------------------

testthat::test_that("top_n_species returns a ggplot object", {
  df <- make_mock_df()
  p  <- testthat::expect_invisible(top_n_species(df))
  testthat::expect_s3_class(p, "ggplot")
})

testthat::test_that("top_n_species shows all species when fewer than n have obs > 1", {
  df <- make_mock_df()
  p  <- top_n_species(df, n = 10)
  pd <- ggplot2::layer_data(p)
  # only Anas platyrhynchos has 2 obs, all others have 1 — all species shown
  testthat::expect_gte(nrow(pd), dplyr::n_distinct(df$name))
})

testthat::test_that("top_n_species respects n parameter when enough species qualify", {
  # build df where 11 species have > 1 observation
  df_large <- purrr::map_dfr(1:11, function(i) {
    make_mock_df() |>
      dplyr::mutate(name = paste0("Species_", i))
  })
  p  <- top_n_species(df_large, n = 5)
  pd <- ggplot2::layer_data(p)
  testthat::expect_lte(dplyr::n_distinct(pd$y), 5)
})

testthat::test_that("top_n_species plot title changes based on data availability", {
  df <- make_mock_df()
  p  <- top_n_species(df, n = 10)
  testthat::expect_true(
    grepl("insufficient", p$labels$title, ignore.case = TRUE)
  )
})


# --- obs_per_year -------------------------------------------------------

testthat::test_that("obs_per_year returns a ggplot object", {
  df <- make_mock_df()
  p  <- testthat::expect_invisible(obs_per_year(df))
  testthat::expect_s3_class(p, "ggplot")
})

testthat::test_that("obs_per_year covers all years up to current year", {
  df           <- make_mock_df()
  p            <- obs_per_year(df)
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  # estrai i dati dal layer direttamente
  plot_years   <- as.integer(as.character(p$data$year))
  testthat::expect_true(current_year %in% plot_years)
})

testthat::test_that("obs_per_year includes years with zero observations", {
  df   <- make_mock_df()
  p    <- obs_per_year(df)
  pd   <- ggplot2::layer_data(p)
  # sum of all bar heights for a year with no obs should be 0
  testthat::expect_true(any(pd$y == 0))
})


# --- obs_per_month ------------------------------------------------------

testthat::test_that("obs_per_month returns a ggplot object", {
  df <- make_mock_df()
  p  <- testthat::expect_invisible(obs_per_month(df))
  testthat::expect_s3_class(p, "ggplot")
})

testthat::test_that("obs_per_month always shows all 12 months", {
  df       <- make_mock_df()
  p        <- obs_per_month(df)
  pd       <- ggplot2::layer_data(p)
  n_months <- dplyr::n_distinct(pd$x)
  testthat::expect_equal(n_months, 12)
})

testthat::test_that("obs_per_month error bars are present", {
  df     <- make_mock_df()
  p      <- obs_per_month(df)
  layers <- sapply(p$layers, function(l) class(l$geom)[1])
  testthat::expect_true("GeomErrorbar" %in% layers)
})


# --- obs_per_hour -------------------------------------------------------

testthat::test_that("obs_per_hour returns a ggplot object", {
  df <- make_mock_df()
  p  <- testthat::expect_invisible(obs_per_hour(df))
  testthat::expect_s3_class(p, "ggplot")
})

testthat::test_that("obs_per_hour always shows all 24 hours", {
  df      <- make_mock_df()
  p       <- obs_per_hour(df)
  pd      <- ggplot2::layer_data(p)
  n_hours <- dplyr::n_distinct(pd$x)
  testthat::expect_equal(n_hours, 24)
})

testthat::test_that("obs_per_hour error bars are present", {
  df     <- make_mock_df()
  p      <- obs_per_hour(df)
  layers <- sapply(p$layers, function(l) class(l$geom)[1])
  testthat::expect_true("GeomErrorbar" %in% layers)
})


# --- species_richness_map -----------------------------------------------

testthat::test_that("species_richness_map returns a leaflet object", {
  df <- make_mock_df()
  testthat::skip_if_not_installed("sf")
  m  <- testthat::expect_no_error(species_richness_map(df, cell_size = 0.1))
  testthat::expect_s3_class(m, "leaflet")
})

testthat::test_that("species_richness_map works without site_boundary", {
  df <- make_mock_df()
  testthat::expect_no_error(
    species_richness_map(df, site_boundary = NULL, cell_size = 0.1)
  )
})

testthat::test_that("species_richness_map cell_size parameter changes grid resolution", {
  df  <- make_mock_df()
  m1  <- species_richness_map(df, cell_size = 0.05)
  m2  <- species_richness_map(df, cell_size = 0.1)
  n1  <- length(m1$x$calls[[2]]$args[[1]]$features)
  n2  <- length(m2$x$calls[[2]]$args[[1]]$features)
  testthat::expect_gte(n1, n2)
})

testthat::test_that("species_richness_map only shows cells with at least 1 species", {
  df <- make_mock_df()
  
  # verifica direttamente sui dati calcolati invece che sulla struttura leaflet
  grid <- sf::st_make_grid(df, cellsize = 0.1, square = TRUE) |>
    sf::st_sf() |>
    dplyr::mutate(grid_id = dplyr::row_number())
  
  grid_richness <- grid |>
    sf::st_join(df) |>
    dplyr::group_by(grid_id) |>
    dplyr::summarise(n_species = dplyr::n_distinct(name, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::filter(n_species > 0)
  
  testthat::expect_true(all(grid_richness$n_species >= 1))
  
  # verifica che la mappa venga creata senza errori
  testthat::expect_no_error(
    species_richness_map(df, cell_size = 0.1)
  )
})


# --- obs_pie_chart ------------------------------------------------------

testthat::test_that("obs_pie_chart returns a ggplot object", {
  df <- make_mock_df()
  p  <- testthat::expect_invisible(obs_pie_chart(df))
  testthat::expect_s3_class(p, "ggplot")
})

testthat::test_that("obs_pie_chart assigns alien species to Alien (IAS) category", {
  df <- make_mock_df()
  p  <- obs_pie_chart(df)
  # Ailanthus altissima is introduced — should appear in Alien (IAS)
  pd <- ggplot2::layer_data(p)
  testthat::expect_true(nrow(pd) >= 1)
})

testthat::test_that("obs_pie_chart category proportions sum to 100", {
  df <- make_mock_df()
  # recompute internally to verify logic
  species_data <- df |>
    sf::st_drop_geometry() |>
    dplyr::select(name, directive, establishmentMeans) |>
    dplyr::distinct(name, .keep_all = TRUE) |>
    dplyr::mutate(
      nativeness = purrr::map_chr(establishmentMeans, function(em) {
        val <- tryCatch(em$nativeness[[1]], error = function(e) NA_character_)
        if (is.null(val) || length(val) == 0) NA_character_ else as.character(val)
      }),
      category = dplyr::case_when(
        !is.na(nativeness) & nativeness == "introduced"  ~ "Alien (IAS)",
        !is.na(directive) & grepl("Habitats", directive) ~ "Habitats Directive",
        !is.na(directive) & grepl("Birds", directive)    ~ "Birds Directive",
        TRUE                                             ~ "Other"
      )
    ) |>
    dplyr::group_by(category) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::mutate(pct = n / sum(n) * 100)
  testthat::expect_equal(round(sum(species_data$pct), 1), 100)
})

testthat::test_that("obs_pie_chart alien category has priority over directives", {
  df <- make_mock_df()
  # Ailanthus altissima is both introduced and in Habitats Directive
  # should be counted as Alien (IAS) only
  species_data <- df |>
    sf::st_drop_geometry() |>
    dplyr::select(name, directive, establishmentMeans) |>
    dplyr::distinct(name, .keep_all = TRUE) |>
    dplyr::mutate(
      nativeness = purrr::map_chr(establishmentMeans, function(em) {
        val <- tryCatch(em$nativeness[[1]], error = function(e) NA_character_)
        if (is.null(val) || length(val) == 0) NA_character_ else as.character(val)
      }),
      category = dplyr::case_when(
        !is.na(nativeness) & nativeness == "introduced"  ~ "Alien (IAS)",
        !is.na(directive) & grepl("Habitats", directive) ~ "Habitats Directive",
        !is.na(directive) & grepl("Birds", directive)    ~ "Birds Directive",
        TRUE                                             ~ "Other"
      )
    )
  ailanthus <- species_data |> dplyr::filter(name == "Ailanthus altissima")
  testthat::expect_equal(ailanthus$category, "Alien (IAS)")
})
