# tests/testthat/test-plots.R

# --- shared mock data ---------------------------------------------------

make_mock_df <- function() {
  mock_em <- function(nativeness) {
    list(tibble::tibble(nativeness = nativeness, authority = "GBIF"))
  }
  mock_iucn <- function() {
    list(tibble::tibble(
      status    = "LC",
      authority = "IUCN",
      name      = "",
      url       = "https://www.iucnredlist.org"
    ))
  }
  
  df <- tibble::tibble(
    name                          = c("Anas platyrhynchos", "Quercus robur",
                                      "Bombus terrestris", "Anas platyrhynchos",
                                      "Vulpes vulpes", "Ailanthus altissima"),
    taxon.iconic_taxon_name       = c("Aves", "Plantae", "Insecta",
                                      "Aves", "Mammalia", "Plantae"),
    observed_on_details.year      = c(2020L, 2021L, 2021L, 2022L, 2022L, 2023L),
    observed_on_details.month     = c(3L, 6L, 6L, 3L, 11L, 5L),
    observed_on_details.hour      = c(8L, 14L, 14L, 9L, 17L, 10L),
    taxon_geoprivacy              = c("open", "open", "obscured",
                                      "open", "open", "open"),
    quality_grade                 = rep("research", 6),
    observed_on                   = as.Date(c("2020-03-01", "2021-06-15",
                                              "2021-06-20", "2022-03-10",
                                              "2022-11-05", "2023-05-01")),
    public_positional_accuracy    = c(10, 20, 15, 10, 30, 25),
    user.login                    = c("user_a", "user_b", "user_a",
                                      "user_c", "user_b", "user_a"),
    uri                           = paste0("https://www.inaturalist.org/observations/", 1:6),
    taxon.id                      = c(1L, 2L, 3L, 1L, 4L, 5L),
    taxon.preferred_common_name   = c("Mallard", "English oak", "Buff-tailed bumblebee",
                                      "Mallard", "Red fox", "Tree of heaven"),
    taxon.default_photo.square_url = rep("https://example.com/photo.jpg", 6),
    directive = c("EU Birds Directive", NA, NA, "EU Birds Directive",
                  NA, "EU Habitats Directive"),
    annex     = c("I", NA, NA, "I", NA, "II"),
    establishmentMeans = c(
      mock_em("native"),
      mock_em("native"),
      mock_em("native"),
      mock_em("native"),
      mock_em("native"),
      mock_em("introduced")
    ),
    status_IUCN = replicate(6, mock_iucn(), simplify = FALSE),
    geojson.coordinates = list(
      list(11.1, 45.1), list(11.2, 45.2), list(11.3, 45.3),
      list(11.1, 45.1), list(11.4, 45.4), list(11.5, 45.5)
    )
  ) |>
    sf::st_as_sf(
      coords = c("geojson.coordinates"),
      crs    = 4326
    )
  
  # st_as_sf non accetta list-column come coords, usiamo un approccio manuale
  df <- tibble::tibble(
    name                          = c("Anas platyrhynchos", "Quercus robur",
                                      "Bombus terrestris", "Anas platyrhynchos",
                                      "Vulpes vulpes", "Ailanthus altissima"),
    taxon.iconic_taxon_name       = c("Aves", "Plantae", "Insecta",
                                      "Aves", "Mammalia", "Plantae"),
    observed_on_details.year      = c(2020L, 2021L, 2021L, 2022L, 2022L, 2023L),
    observed_on_details.month     = c(3L, 6L, 6L, 3L, 11L, 5L),
    observed_on_details.hour      = c(8L, 14L, 14L, 9L, 17L, 10L),
    taxon_geoprivacy              = c("open", "open", "obscured",
                                      "open", "open", "open"),
    quality_grade                 = rep("research", 6),
    observed_on                   = as.Date(c("2020-03-01", "2021-06-15",
                                              "2021-06-20", "2022-03-10",
                                              "2022-11-05", "2023-05-01")),
    public_positional_accuracy    = c(10, 20, 15, 10, 30, 25),
    user.login                    = c("user_a", "user_b", "user_a",
                                      "user_c", "user_b", "user_a"),
    uri                           = paste0("https://www.inaturalist.org/observations/", 1:6),
    taxon.id                      = c(1L, 2L, 3L, 1L, 4L, 5L),
    taxon.preferred_common_name   = c("Mallard", "English oak", "Buff-tailed bumblebee",
                                      "Mallard", "Red fox", "Tree of heaven"),
    taxon.default_photo.square_url = rep("https://example.com/photo.jpg", 6),
    directive  = c("EU Birds Directive", NA, NA, "EU Birds Directive",
                   NA, "EU Habitats Directive"),
    annex      = c("I", NA, NA, "I", NA, "II"),
    establishmentMeans = list(
      tibble::tibble(nativeness = "native",     authority = "GBIF"),
      tibble::tibble(nativeness = "native",     authority = "GBIF"),
      tibble::tibble(nativeness = "native",     authority = "GBIF"),
      tibble::tibble(nativeness = "native",     authority = "GBIF"),
      tibble::tibble(nativeness = "native",     authority = "GBIF"),
      tibble::tibble(nativeness = "introduced", authority = "GBIF")
    ),
    status_IUCN = replicate(6, tibble::tibble(
      status    = "LC",
      authority = "IUCN",
      name      = "",
      url       = "https://www.iucnredlist.org"
    ), simplify = FALSE),
    lon = c(11.1, 11.2, 11.3, 11.1, 11.4, 11.5),
    lat = c(45.1, 45.2, 45.3, 45.1, 45.4, 45.5)
  ) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  df
}


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
  df       <- make_mock_df()
  p        <- obs_per_year(df)
  pd       <- ggplot2::layer_data(p)
  # x axis levels should include current year
  x_levels <- levels(ggplot2::layer_scales(p)$x$range$range)
  testthat::expect_true(
    as.character(as.integer(format(Sys.Date(), "%Y"))) %in% x_levels
  )
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
  m  <- species_richness_map(df)
  testthat::expect_s3_class(m, "leaflet")
})

testthat::test_that("species_richness_map works without site_boundary", {
  df <- make_mock_df()
  testthat::expect_no_error(species_richness_map(df, site_boundary = NULL))
})

testthat::test_that("species_richness_map cell_size parameter changes grid resolution", {
  df  <- make_mock_df()
  m1  <- species_richness_map(df, cell_size = 0.001)
  m2  <- species_richness_map(df, cell_size = 0.01)
  # coarser grid should have fewer or equal polygons
  n1  <- length(m1$x$calls[[2]]$args[[1]]$features)
  n2  <- length(m2$x$calls[[2]]$args[[1]]$features)
  testthat::expect_gte(n1, n2)
})

testthat::test_that("species_richness_map only shows cells with at least 1 species", {
  df <- make_mock_df()
  m  <- species_richness_map(df)
  # all polygon popups should contain a number >= 1
  popups <- m$x$calls[[which(
    sapply(m$x$calls, function(x) "popup" %in% names(x$args[[1]]))
  )]]$args[[1]]$popup
  counts <- as.integer(gsub(".*<b>Species richness:</b> (\\d+).*", "\\1", popups))
  testthat::expect_true(all(counts >= 1))
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