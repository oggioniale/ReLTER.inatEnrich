#' Plot number of observations and species per iconic taxonomic group
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Takes a tibble of iNaturalist occurrences and produces a grouped bar chart
#' showing the total number of observations and unique species for each iconic
#' taxonomic group as defined by iNaturalist.
#'
#' @param df An \code{sf} tibble of iNaturalist occurrences containing at least
#'   the columns \code{taxon.iconic_taxon_name} and \code{name}.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object printed to the active device.
#'
#' @author Alessandro Oggioni, PhD \email{alessandro.oggioni@@cnr.it}
#' @author Alice Lenzi, phD \email{alice.lenzi@@crea.gov.it}
#'
#' @seealso
#' \code{\link{top_n_species}}, \code{\link{obs_per_year}}
#'
#' @export
#'
#' @importFrom dplyr group_by summarise n n_distinct arrange desc mutate
#' @importFrom ggplot2 ggplot aes geom_col geom_text position_dodge
#'   scale_fill_manual scale_y_continuous expansion labs theme_minimal
#'   theme element_text
#'
#' @examples
#' \dontrun{
#' iconic_taxa(occ_eLTER_legal)
#' }
#'
### iconic_taxa
iconic_taxa <- function(df) {
  # summarize
  summary_df <- df |>
    dplyr::group_by(iconic_taxon = `taxon.iconic_taxon_name`) |>
    dplyr::summarise(
      n_observations = dplyr::n(),
      n_species      = dplyr::n_distinct(`name`, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(n_observations))
  # DF for plot
  plot_df <- rbind(
    data.frame(iconic_taxon = summary_df$iconic_taxon,
               metric       = "Observations",
               value        = summary_df$n_observations),
    data.frame(iconic_taxon = summary_df$iconic_taxon,
               metric       = "Species",
               value        = summary_df$n_species)
  ) |>
    dplyr::mutate(
      metric = factor(metric, levels = c("Observations", "Species"))
    )
  # plot
  p <- ggplot2::ggplot(plot_df,
                       ggplot2::aes(x    = reorder(iconic_taxon, -value * (metric == "Observations")),
                                    y    = value,
                                    fill = metric)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.7), width = 0.65) +
    ggplot2::geom_text(ggplot2::aes(label = format(value, big.mark = " ", scientific = FALSE)),
                       position = ggplot2::position_dodge(width = 0.7),
                       vjust = -0.4, size = 3) +
    ggplot2::scale_fill_manual(
      values = c("Observations" = "#1D9E75",
                 "Species"      = "#7F77DD"),
      labels = c("Observations" = "N. of observations",
                 "Species"      = "N. of species"),
      name = NULL
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15)),
                                labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
    ggplot2::labs(title = "Numer of observations and species per iconic taxonomic group",
                  x     = NULL,
                  y     = "Count") +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      axis.text.x        = ggplot2::element_text(angle = 35, hjust = 1),
      legend.position    = "right",
      panel.grid.major.y = ggplot2::element_blank()
    )
  # output
  print(p)
  invisible(p)
}

#' Plot top N most observed species
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Takes a tibble of iNaturalist occurrences and produces a horizontal bar
#' chart of the top \code{n} most observed species, coloured by iconic taxon
#' group. If fewer than \code{n} species have more than one observation, all
#' species are shown.
#'
#' @param df An \code{sf} tibble of iNaturalist occurrences containing at least
#'   the columns \code{name} and \code{taxon.iconic_taxon_name}.
#' @param n Integer. Number of top species to display. Default is \code{10}.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object printed to the active device.
#'
#' @author Alessandro Oggioni, PhD \email{alessandro.oggioni@@cnr.it}
#' @author Alice Lenzi, phD \email{alice.lenzi@@crea.gov.it}
#' 
#' @seealso
#' \code{\link{iconic_taxa}}, \code{\link{obs_per_year}}
#'
#' @export
#'
#' @importFrom dplyr filter group_by summarise n arrange desc slice_head
#'   coalesce
#' @importFrom grDevices hcl.colors
#' @importFrom stats setNames
#' @importFrom ggplot2 ggplot aes geom_col geom_text scale_fill_manual
#'   scale_y_continuous expansion coord_flip labs theme_minimal theme
#'   element_text
#'
#' @examples
#' \dontrun{
#' top_n_species(occ_eLTER_legal)
#' top_n_species(occ_eLTER_legal, n = 20)
#' }
#'
### top_n_species
top_n_species <- function(df, n = 10) {
  iconic_levels <- sort(unique(dplyr::coalesce(df$`taxon.iconic_taxon_name`, "Unknown")))
  pal_colors    <- grDevices::hcl.colors(length(iconic_levels), "Set 2")
  iconic_colors <- stats::setNames(pal_colors, iconic_levels)
  
  species_obs <- df |>
    dplyr::filter(!is.na(`name`), !is.na(`taxon.iconic_taxon_name`)) |>
    dplyr::group_by(species      = `name`,
                    iconic_taxon = `taxon.iconic_taxon_name`) |>
    dplyr::summarise(n_obs = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(n_obs), species)
  
  above_one <- species_obs |> dplyr::filter(n_obs > 1)
  
  top_n <- if (nrow(above_one) >= n) {
    dplyr::slice_head(above_one, n = n)
  } else {
    species_obs
  }
  
  plot_title <- if (nrow(above_one) >= n) {
    paste("Top", n, "most observed species")
  } else {
    "All species (insufficient data for top 10)"
  }
  
  p <- ggplot2::ggplot(top_n,
                       ggplot2::aes(x    = reorder(species, n_obs),
                                    y    = n_obs,
                                    fill = iconic_taxon)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = n_obs),
                       hjust = -0.2, size = 3) +
    ggplot2::scale_fill_manual(values = iconic_colors, name = "Iconic taxon") +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.15)),
      breaks = function(x) seq(0, ceiling(max(x)), by = max(1, ceiling(max(x) / 5)))
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = plot_title,
                  x     = NULL,
                  y     = "Observations") +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(face = "italic"),
      legend.position = "right",
      panel.grid.major.y = ggplot2::element_blank()
    )
  
  print(p)
  invisible(p)
}

#' Plot number of observations per year
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Takes a tibble of iNaturalist occurrences and produces a stacked bar chart
#' of observations per year, coloured by iconic taxon group. All years from
#' the first observation to the current year are shown, including years with
#' zero observations.
#'
#' @param df An \code{sf} tibble of iNaturalist occurrences containing at least
#'   the columns \code{observed_on_details.year} and
#'   \code{taxon.iconic_taxon_name}.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object printed to the active device.
#'
#' @author Alessandro Oggioni, PhD \email{alessandro.oggioni@@cnr.it}
#' @author Alice Lenzi, phD \email{alice.lenzi@@crea.gov.it}
#' 
#' @seealso
#' \code{\link{obs_per_month}}, \code{\link{obs_per_hour}}
#'
#' @export
#'
#' @importFrom dplyr filter group_by summarise n left_join mutate coalesce
#' @importFrom grDevices hcl.colors
#' @importFrom stats setNames
#' @importFrom ggplot2 ggplot aes geom_col scale_fill_manual scale_y_continuous
#'   expansion labs theme_minimal theme element_text
#'
#' @examples
#' \dontrun{
#' obs_per_year(occ_eLTER_legal)
#' }
#'
### obs_per_year
obs_per_year <- function(df) {
  
  iconic_levels <- sort(unique(dplyr::coalesce(df$`taxon.iconic_taxon_name`, "Unknown")))
  pal_colors    <- grDevices::hcl.colors(length(iconic_levels), "Set 2")
  iconic_colors <- stats::setNames(pal_colors, iconic_levels)
  
  year_obs <- df |>
    dplyr::filter(!is.na(`observed_on_details.year`),
                  !is.na(`taxon.iconic_taxon_name`)) |>
    dplyr::group_by(year = `observed_on_details.year`,
                    iconic_taxon = `taxon.iconic_taxon_name`) |>
    dplyr::summarise(n_obs = dplyr::n(), .groups = "drop")
  
  all_combinations <- expand.grid(
    year = seq(min(year_obs$year), as.integer(format(Sys.Date(), "%Y")), by = 1),
    iconic_taxon = iconic_levels,
    stringsAsFactors = FALSE
  )
  
  year_obs <- dplyr::left_join(
    all_combinations, year_obs,
    by = c("year", "iconic_taxon")
    ) |>
    dplyr::mutate(n_obs = ifelse(is.na(n_obs), 0L, n_obs))
  
  all_years <- sort(unique(year_obs$year))
  step      <- max(1, floor(length(all_years) / 10))
  years_to_show <- as.character(all_years[seq(1, length(all_years), by = step)])
  
  year_totals <- year_obs |>
    dplyr::group_by(year) |>
    dplyr::summarise(total = sum(n_obs), .groups = "drop") |>
    dplyr::filter(total > 0)
  
  p <- ggplot2::ggplot(year_obs,
                       ggplot2::aes(x = factor(year), y = n_obs, fill = iconic_taxon)) +
    ggplot2::geom_col(width = 0.6) +
    ggplot2::geom_text(
      data = year_totals,
      ggplot2::aes(x = factor(year), y = total, label = total, fill = NULL),
      vjust = -0.4, size = 3, color = "grey30"
    ) +
    ggplot2::scale_fill_manual(values = iconic_colors, name = "Iconic taxon") +
    ggplot2::scale_x_discrete(breaks = years_to_show) +
    ggplot2::scale_y_continuous(
      expand   = ggplot2::expansion(mult = c(0, 0.15)),
      n.breaks = 5
    ) +
    ggplot2::labs(title = "Observations per year",
                  x     = NULL,
                  y     = "Observations") +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      axis.text.x        = ggplot2::element_text(angle = 35, hjust = 1),
      legend.position    = "right",
      panel.grid.major.x = ggplot2::element_blank()
    )
  
  print(p)
  invisible(p)
}

#' Plot mean number of observations per month
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Takes a tibble of iNaturalist occurrences and produces a stacked bar chart
#' of mean observations per calendar month across years, coloured by iconic
#' taxon group. Error bars show ± 1 standard error of the mean across years.
#' All twelve months are always shown, including those with zero observations.
#'
#' @param df An \code{sf} tibble of iNaturalist occurrences containing at least
#'   the columns \code{observed_on_details.month},
#'   \code{observed_on_details.year}, and \code{taxon.iconic_taxon_name}.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object printed to the active device.
#'
#' @author Alessandro Oggioni, PhD \email{alessandro.oggioni@@cnr.it}
#' @author Alice Lenzi, phD \email{alice.lenzi@@crea.gov.it}
#' 
#' @seealso
#' \code{\link{obs_per_year}}, \code{\link{obs_per_hour}}
#'
#' @export
#'
#' @importFrom dplyr filter group_by summarise n n_distinct left_join mutate
#'   coalesce
#' @importFrom grDevices hcl.colors
#' @importFrom stats setNames sd
#' @importFrom ggplot2 ggplot aes geom_col geom_errorbar scale_fill_manual
#'   scale_y_continuous expansion labs theme_minimal theme element_text
#'
#' @examples
#' \dontrun{
#' obs_per_month(occ_eLTER_legal)
#' }
#'
### obs_per_month
obs_per_month <- function(df) {
  
  month_labels  <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  iconic_levels <- sort(unique(dplyr::coalesce(df$`taxon.iconic_taxon_name`, "Unknown")))
  pal_colors    <- grDevices::hcl.colors(length(iconic_levels), "Set 2")
  iconic_colors <- stats::setNames(pal_colors, iconic_levels)
  
  month_obs <- df |>
    dplyr::filter(!is.na(`observed_on_details.month`),
                  !is.na(`observed_on_details.year`),
                  !is.na(`taxon.iconic_taxon_name`)) |>
    dplyr::group_by(year         = `observed_on_details.year`,
                    month        = `observed_on_details.month`,
                    iconic_taxon = `taxon.iconic_taxon_name`) |>
    dplyr::summarise(n_obs = dplyr::n(), .groups = "drop")
  
  all_combinations <- expand.grid(
    year         = unique(month_obs$year),
    month        = 1:12,
    iconic_taxon = iconic_levels,
    stringsAsFactors = FALSE
  )
  
  month_obs <- dplyr::left_join(all_combinations, month_obs,
                                by = c("year", "month", "iconic_taxon")) |>
    dplyr::mutate(n_obs = ifelse(is.na(n_obs), 0L, n_obs))
  
  month_mean_taxon <- month_obs |>
    dplyr::group_by(month, iconic_taxon) |>
    dplyr::summarise(mean_obs = mean(n_obs), .groups = "drop") |>
    dplyr::mutate(month_label = factor(month_labels[month], levels = month_labels))
  
  month_stats <- month_obs |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      mean_tot = mean(tapply(n_obs, year, sum)),
      se_tot   = stats::sd(tapply(n_obs, year, sum)) / sqrt(dplyr::n_distinct(year)),
      .groups  = "drop"
    ) |>
    dplyr::mutate(month_label = factor(month_labels[month], levels = month_labels))
  
  month_totals <- month_stats |>
    dplyr::filter(mean_tot > 0) |>
    dplyr::mutate(label = round(mean_tot, 1))
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_col(
      data = month_mean_taxon,
      ggplot2::aes(x = month_label, y = mean_obs, fill = iconic_taxon),
      width = 0.6
    ) +
    ggplot2::geom_errorbar(
      data = month_stats,
      ggplot2::aes(x    = month_label,
                   ymin = pmax(mean_tot - se_tot, 0),
                   ymax = mean_tot + se_tot),
      width = 0.25, color = "black", linewidth = 0.7
    ) +
    # ggplot2::geom_text(
    #   data = month_totals,
    #   ggplot2::aes(x = month_label, y = mean_tot + se_tot, label = label),
    #   vjust = -0.4, size = 3, color = "grey30"
    # ) +
    ggplot2::scale_fill_manual(values = iconic_colors, name = "Iconic taxon") +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.15))
    ) +
    ggplot2::labs(title   = "Mean observations per month",
                  caption = "Bars: mean across years | Error bars: ± SE",
                  x       = NULL,
                  y       = "Mean observations") +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      legend.position    = "right",
      panel.grid.major.x = ggplot2::element_blank(),
      plot.caption       = ggplot2::element_text(size = 10, color = "grey50")
    )
  
  print(p)
  invisible(p)
}

#' Plot mean number of observations per hour of the day
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Takes a tibble of iNaturalist occurrences and produces a stacked bar chart
#' of mean observations per hour of the day across years, coloured by iconic
#' taxon group. Error bars show ± 1 standard error of the mean across years.
#' All 24 hours are always shown, including those with zero observations.
#'
#' @param df An \code{sf} tibble of iNaturalist occurrences containing at least
#'   the columns \code{observed_on_details.hour},
#'   \code{observed_on_details.year}, and \code{taxon.iconic_taxon_name}.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object printed to the active device.
#'
#' @author Alessandro Oggioni, PhD \email{alessandro.oggioni@@cnr.it}
#' @author Alice Lenzi, phD \email{alice.lenzi@@crea.gov.it}
#' 
#' @seealso
#' \code{\link{obs_per_year}}, \code{\link{obs_per_month}}
#'
#' @export
#'
#' @importFrom dplyr filter group_by summarise n n_distinct left_join mutate
#'   coalesce
#' @importFrom grDevices hcl.colors
#' @importFrom stats setNames sd
#' @importFrom ggplot2 ggplot aes geom_col geom_errorbar scale_fill_manual
#'   scale_y_continuous expansion labs theme_minimal theme element_text
#'
#' @examples
#' \dontrun{
#' obs_per_hour(occ_eLTER_legal)
#' }
#'
### obs_per_hour
obs_per_hour <- function(df) {
  iconic_levels <- sort(unique(dplyr::coalesce(df$`taxon.iconic_taxon_name`, "Unknown")))
  pal_colors    <- grDevices::hcl.colors(length(iconic_levels), "Set 2")
  iconic_colors <- stats::setNames(pal_colors, iconic_levels)
  
  hour_obs <- df |>
    dplyr::filter(!is.na(`observed_on_details.hour`),
                  !is.na(`observed_on_details.year`),
                  !is.na(`taxon.iconic_taxon_name`)) |>
    dplyr::group_by(year         = `observed_on_details.year`,
                    hour         = `observed_on_details.hour`,
                    iconic_taxon = `taxon.iconic_taxon_name`) |>
    dplyr::summarise(n_obs = dplyr::n(), .groups = "drop")
  
  all_combinations <- expand.grid(
    year         = unique(hour_obs$year),
    hour         = 0:23,
    iconic_taxon = iconic_levels,
    stringsAsFactors = FALSE
  )
  
  hour_obs <- dplyr::left_join(all_combinations, hour_obs,
                               by = c("year", "hour", "iconic_taxon")) |>
    dplyr::mutate(n_obs = ifelse(is.na(n_obs), 0L, n_obs))
  
  hour_mean_taxon <- hour_obs |>
    dplyr::group_by(hour, iconic_taxon) |>
    dplyr::summarise(mean_obs = mean(n_obs), .groups = "drop") |>
    dplyr::mutate(hour_label = factor(sprintf("%02d:00", hour),
                                      levels = sprintf("%02d:00", 0:23)))
  
  hour_stats <- hour_obs |>
    dplyr::group_by(hour) |>
    dplyr::summarise(
      mean_tot = mean(tapply(n_obs, year, sum)),
      se_tot   = stats::sd(tapply(n_obs, year, sum)) / sqrt(dplyr::n_distinct(year)),
      .groups  = "drop"
    ) |>
    dplyr::mutate(hour_label = factor(sprintf("%02d:00", hour),
                                      levels = sprintf("%02d:00", 0:23)))
  
  hour_totals <- hour_stats |>
    dplyr::filter(mean_tot > 0) |>
    dplyr::mutate(label = round(mean_tot, 1))
  
  p <- ggplot2::ggplot() +
    ggplot2::geom_col(
      data = hour_mean_taxon,
      ggplot2::aes(x = hour_label, y = mean_obs, fill = iconic_taxon),
      width = 0.6
    ) +
    ggplot2::geom_errorbar(
      data = hour_stats,
      ggplot2::aes(x    = hour_label,
                   ymin = pmax(mean_tot - se_tot, 0),
                   ymax = mean_tot + se_tot),
      width = 0.25, color = "black", linewidth = 0.7
    ) +
    # ggplot2::geom_text(
    #   data = hour_totals,
    #   ggplot2::aes(x = hour_label, y = mean_tot + se_tot, label = label),
    #   vjust = -0.4, size = 3, color = "grey30"
    # ) +
    ggplot2::scale_fill_manual(values = iconic_colors, name = "Iconic taxon") +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.15))
    ) +
    ggplot2::labs(title   = "Mean observations per hour of the day",
                  caption = "Bars: mean across years | Error bars: ± SE",
                  x       = NULL,
                  y       = "Mean observations") +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      axis.text.x        = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
      legend.position    = "right",
      panel.grid.major.x = ggplot2::element_blank(),
      plot.caption       = ggplot2::element_text(size = 10, color = "grey50")
    )
  
  print(p)
  invisible(p)
}

#' Map species richness on a spatial grid
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Takes a spatial tibble of iNaturalist occurrences and produces an
#' interactive Leaflet map where each grid cell is coloured according to
#' the number of unique species observed within it (species richness).
#' Empty cells are removed from the map.
#'
#' @param df An \code{sf} tibble of iNaturalist occurrences containing at
#'   least the column \code{name} (scientific name) and valid point geometry.
#' @param site_boundary An \code{sf} object representing the eLTER site
#'   boundary polygon. If \code{NULL} (default), no boundary is drawn.
#' @param cell_size Numeric. Size of each grid cell in decimal degrees.
#'   Default is \code{0.001} (approximately 100 m). Use larger values
#'   (e.g. \code{0.005}) for broader spatial extents.
#'
#' @return A \code{\link[leaflet]{leaflet}} map object with:
#'   \itemize{
#'     \item An OpenStreetMap base tile layer.
#'     \item Grid cells coloured by species richness using the
#'       \code{"viridis"} palette.
#'     \item A popup per cell showing the species richness value.
#'     \item A legend for species richness.
#'     \item An optional eLTER site boundary polygon layer.
#'     \item A layers control to toggle map groups.
#'   }
#'
#' @author Alessandro Oggioni, PhD \email{alessandro.oggioni@@cnr.it}
#' @author Alice Lenzi, phD \email{alice.lenzi@@crea.gov.it}
#' 
#' @seealso
#' \code{\link{create_leaflet_occ_map}}
#'
#' @export
#'
#' @importFrom sf st_make_grid st_sf st_join
#' @importFrom dplyr mutate row_number group_by summarise n_distinct filter
#' @importFrom leaflet leaflet addTiles addPolygons addLegend addLayersControl
#'   colorNumeric layersControlOptions
#'
#' @examples
#' \dontrun{
#' deimsid <- "https://deims.org/6b62feb2-61bf-47e1-b97f-0e909c408db8"
#' site_boundary <- ReLTER::get_site_info(deimsid = deimsid)
#'
#' species_richness_map(
#'   df            = occ_eLTER_legal,
#'   site_boundary = site_boundary,
#'   cell_size     = 0.005
#' )
#' }
#'
### species_richness_map
species_richness_map <- function(df, site_boundary = NULL, cell_size = 0.001) {
  grid <- sf::st_make_grid(df, cellsize = cell_size, square = TRUE) |>
    sf::st_sf() |>
    dplyr::mutate(grid_id = dplyr::row_number())
  
  grid_richness <- grid |>
    sf::st_join(df) |>
    dplyr::group_by(grid_id) |>
    dplyr::summarise(n_species = dplyr::n_distinct(name, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::filter(n_species > 0)
  
  pal <- leaflet::colorNumeric(
    palette = "viridis",
    domain  = grid_richness$n_species
  )
  
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
        fillOpacity  = 0.25,
        smoothFactor = 0.5,
        popup = ~paste0(
          "<b>Site title:</b><br>",
          sprintf('<a href="%s" target="_blank">%s</a>', uri, title.x)
        ),
        group = "eLTER site boundary"
      )
  }
  
  map <- map |>
    leaflet::addPolygons(
      data        = grid_richness,
      fillColor   = ~pal(n_species),
      fillOpacity = 0.7,
      color       = NA,
      weight      = 0,
      popup       = ~paste0("<b>Species richness:</b> ", n_species),
      group       = "Species richness"
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      pal      = pal,
      values   = grid_richness$n_species,
      title    = "N. species",
      opacity  = 0.7
    )
  
  if (!is.null(site_boundary)) {
    map <- map |>
      leaflet::addLayersControl(
        overlayGroups = c("Species richness", "eLTER site boundary"),
        options       = leaflet::layersControlOptions(collapsed = FALSE)
      )
  } else {
    map <- map |>
      leaflet::addLayersControl(
        overlayGroups = "Species richness",
        options       = leaflet::layersControlOptions(collapsed = FALSE)
      )
  }
  
  return(map)
}

#' Plot a donut chart of species by conservation category
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Takes a tibble of iNaturalist occurrences enriched with establishment means
#' and EU directive information, and produces a donut chart showing the
#' proportion of species assigned to each conservation category. Each species
#' is assigned to exactly one category following a fixed priority order:
#' Alien (IAS) > Habitats Directive > Birds Directive > Other.
#'
#' @param df An \code{sf} tibble of iNaturalist occurrences containing at least
#'   the columns:
#'   \describe{
#'     \item{name}{character. Scientific name.}
#'     \item{directive}{character. EU directive name, produced by
#'       \code{\link{add_eunis_legal_to_occ}}.}
#'     \item{establishmentMeans}{list-column of tibbles with a \code{nativeness}
#'       field, produced by \code{\link{add_nativeness_to_occ}}.}
#'   }
#'
#' @return A \code{\link[ggplot2]{ggplot}} object printed to the active device.
#'   The total number of species is displayed at the centre of the donut.
#'   Category counts and percentages are shown in the legend.
#'
#' @note
#' Species are assigned to one category only, in order of priority:
#' \enumerate{
#'   \item Alien (IAS) — \code{nativeness == "introduced"}
#'   \item Habitats Directive — \code{directive} contains \code{"Habitats"}
#'   \item Birds Directive — \code{directive} contains \code{"Birds"}
#'   \item Other — all remaining species
#' }
#' A species meeting multiple criteria will appear only in the
#' highest-priority category.
#'
#' @author Alessandro Oggioni, PhD \email{alessandro.oggioni@@cnr.it}
#' @author Alice Lenzi, phD \email{alice.lenzi@@crea.gov.it}
#' 
#' @seealso
#' \code{\link{add_nativeness_to_occ}}, \code{\link{add_eunis_legal_to_occ}}
#'
#' @export
#'
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr select distinct mutate group_by summarise n case_when
#' @importFrom purrr map_chr
#' @importFrom stats setNames
#' @importFrom ggplot2 ggplot aes geom_col annotate coord_polar xlim
#'   scale_fill_manual labs theme_void theme element_text unit
#'
#' @examples
#' \dontrun{
#' obs_pie_chart(occ_eLTER_legal)
#' }
#'
### obs_pie_chart
obs_pie_chart <- function(df) {
  
  required_cols <- c(
    "status_IUCN",
    "establishmentMeans",
    "directive", "annex"
  )
  
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("The following required columns are missing: ", paste(missing_cols, collapse = ", "),
         "\nRun the enrichment pipeline first.")
  }
  
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
        !is.na(nativeness) & nativeness == "introduced" ~ "Alien (IAS)",
        !is.na(directive) & grepl("Habitats", directive) ~ "Habitats Directive",
        !is.na(directive) & grepl("Birds", directive)    ~ "Birds Directive",
        TRUE                                             ~ "Other"
      )
    ) |>
    dplyr::group_by(category) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::mutate(
      pct          = n / sum(n) * 100,
      legend_label = paste0(category, ": ", n, " spp. (", round(pct, 1), "%)")
    )
  
  category_colors <- c(
    "Alien (IAS)"        = "#E24B4A",
    "Habitats Directive" = "#1D9E75",
    "Birds Directive"    = "#378ADD",
    "Other"              = "#888780"
  )
  
  total_species <- sum(species_data$n)
  
  p <- ggplot2::ggplot(species_data,
                       ggplot2::aes(x    = 2,
                                    y    = pct,
                                    fill = category)) +
    ggplot2::geom_col(width = 1, color = "white", linewidth = 0.5) +
    ggplot2::annotate("text",
                      x        = 0,
                      y        = 0,
                      label    = paste0("Total\n", total_species, " spp."),
                      size     = 4.5,
                      fontface = "bold") +
    ggplot2::coord_polar(theta = "y", start = 0) +
    ggplot2::xlim(0, 3) +
    ggplot2::scale_fill_manual(
      values = category_colors,
      name   = NULL,
      labels = stats::setNames(species_data$legend_label, species_data$category)
    ) +
    ggplot2::labs(
      title    = "Species composition by conservation category",
      subtitle = paste0(
        "Each species is assigned to one category only, in order of priority:\n",
        "Alien (IAS) > Habitats Directive > Birds Directive > Other"
      ),
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_void(base_size = 13) +
    ggplot2::theme(
      legend.position  = "right",
      legend.text      = ggplot2::element_text(size = 10),
      legend.key.size  = ggplot2::unit(0.5, "cm"),
      legend.direction = "vertical",
      plot.title       = ggplot2::element_text(size = 13, hjust = 0.5),
      plot.subtitle    = ggplot2::element_text(size = 9, color = "grey50",
                                               hjust = 0.5, lineheight = 1.4)
    )
  
  print(p)
  invisible(p)
}

#' Plot a donut chart of observations contributing to eLTER Standard Observations
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Produces a donut chart showing the number of observations contributing
#' to eLTER Standard Observations SOBIO_014 (Flying insects) and
#' SOBIO_018 (Acoustic recording). Observations can contribute to both
#' SOs simultaneously (e.g. Orthoptera).
#'
#' @param df An \code{sf} tibble of iNaturalist occurrences containing at
#'   least the columns \code{SOBIO_014} and \code{SOBIO_018}, produced by
#'   the enrichment pipeline.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object printed to the active device.
#'
#' @author Alessandro Oggioni, PhD \email{alessandro.oggioni@@cnr.it}
#' @author Alice Lenzi, phD \email{alice.lenzi@@crea.gov.it}
#' 
#' @seealso \code{\link{obs_pie_chart}}
#'
#' @export
#'
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr mutate group_by summarise n
#' @importFrom ggplot2 ggplot aes geom_col annotate coord_polar xlim
#'   scale_fill_manual labs theme_void theme element_text unit
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#' obs_SO_pie_chart(occ_eLTER_legal)
#' }
#'
### obs_SO_pie_chart
obs_SO_pie_chart <- function(df) {
  
  if (!all(c("SOBIO_014", "SOBIO_017", "SOBIO_018") %in% names(df))) {
    stop("Columns 'SOBIO_014', 'SOBIO_017', and 'SOBIO_018' are missing. ",
         "Run the enrichment pipeline first.")
  }
  
  obs_flat <- df |> sf::st_drop_geometry()
  
  n_014 <- sum(obs_flat$SOBIO_014 & !obs_flat$SOBIO_018, na.rm = TRUE)
  n_017 <- sum(obs_flat$SOBIO_017, na.rm = TRUE)
  n_018 <- sum(obs_flat$SOBIO_018 & !obs_flat$SOBIO_014, na.rm = TRUE)
  n_both <- sum(obs_flat$SOBIO_014 &  obs_flat$SOBIO_018, na.rm = TRUE)
  n_neither <- sum(!obs_flat$SOBIO_014 & !obs_flat$SOBIO_018 & !obs_flat$SOBIO_017, na.rm = TRUE)
  total_obs <- nrow(obs_flat)
  
  so_data <- data.frame(
    category = c(
      "Only Flying insects\n(SOBIO_014)",
      "Only Acoustic recording\n(SOBIO_018)",
      "Orthoptera that include SOBIO_014\n& SOBIO_018",
      "Only Vegetation composition\n(SOBIO_017)",
      "No SO"
    ),
    n = c(n_014, n_018, n_017, n_both, n_neither),
    stringsAsFactors = FALSE
  ) |>
    dplyr::mutate(
      pct          = n / sum(n) * 100,
      legend_label = paste0(
        gsub("\n", " ", category), ": ",
        n, " obs. (", round(pct, 1), "%)"
      )
    )
  
  so_colors <- c(
    "Only Flying insects\n(SOBIO_014)" = "#639922",
    "Only Acoustic recording\n(SOBIO_018)"  = "#378ADD",
    "Orthoptera that include SOBIO_014\n& SOBIO_018" = "#7F77DD",
    "Only Vegetation composition\n(SOBIO_017)" = "#EEB565",
    "No SO" = "#888780"
  )
  
  p <- ggplot2::ggplot(so_data,
                       ggplot2::aes(x    = 2,
                                    y    = pct,
                                    fill = category)) +
    ggplot2::geom_col(width = 1, color = "white", linewidth = 0.5) +
    ggplot2::annotate("text",
                      x = 0,
                      y = 0,
                      label = paste0("Total\n", total_obs, " obs."),
                      size = 4.5,
                      fontface = "bold") +
    ggplot2::coord_polar(theta = "y", start = 0) +
    ggplot2::xlim(0, 3) +
    ggplot2::scale_fill_manual(
      values = so_colors,
      name   = NULL,
      labels = stats::setNames(so_data$legend_label, so_data$category)
    ) +
    ggplot2::labs(
      title    = "Observations contributing to eLTER Standard Observations",
      subtitle = paste0(
        "SOBIO_014: Flying insects (all the insects) | ",
        "SOBIO_018: Acoustic recording (birds, bats, amphibians, orthoptera)\n",
        "SOBIO_017: Vegetation composition (all plants)\n",
        "Orthoptera contribute to SOBIO_014 and SOBIO_018"
      ),
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_void(base_size = 13) +
    ggplot2::theme(
      legend.position = "right",
      legend.text = ggplot2::element_text(size = 10),
      legend.key.size = ggplot2::unit(0.5, "cm"),
      legend.direction = "vertical",
      plot.title = ggplot2::element_text(size = 13, hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 9, color = "grey50",
                                               hjust = 0.5, lineheight = 1.4)
    )
  
  print(p)
  invisible(p)
}
