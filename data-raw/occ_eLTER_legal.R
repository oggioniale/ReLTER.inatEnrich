## code to prepare `occ_eLTER_legal` and `site_boundary` datasets

library(ReLTER.inatEnrich)

# eLTER site DEIMS-ID — Gran Paradiso
deimsid <- "https://deims.org/15c3e841-8494-42d2-a44e-c49a0ff25946"

# Site boundary
site_boundary <- ReLTER::get_site_info(deimsid = deimsid)

# Download iNaturalist occurrences
occ_eLTER <- ReLTER::get_site_speciesOccurrences(
  deimsid  = deimsid,
  list_DS  = "inat",
  show_map = FALSE,
  limit = 5000
)

# Clip occurrences to fit within the site boundary
occ_in_site <- sf::st_intersection(
  x = iNat_occ_eLTER_site$inat,
  y = site_boundary
)

# Enrich with IUCN conservation status
occ_eLTER_iucn <- add_iucn_to_occ(
  occ_eLTER = occ_eLTER$inat
)

# Enrich with establishment means
occ_eLTER_nativeness <- add_nativeness_to_occ(
  occ_eLTER = occ_eLTER_iucn,
  country = site_boundary$country
)

# Enrich with EUNIS legal information
occ_eLTER_legal <- add_eunis_legal_to_occ(
  occ_eLTER = occ_eLTER_nativeness
)

# Save datasets
usethis::use_data(occ_eLTER_legal, overwrite = TRUE)
usethis::use_data(site_boundary, overwrite = TRUE)
