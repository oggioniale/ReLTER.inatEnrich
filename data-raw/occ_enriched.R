## code to prepare `occ_eLTER_enrich` and `site_boundary` datasets

library(ReLTER)
library(ReLTER.inatEnrich)

# eLTER site DEIMS-ID — Gran Paradiso
deimsid <- "https://deims.org/15c3e841-8494-42d2-a44e-c49a0ff25946"
deimsid <- "https://deims.org/15c3e841-8494-42d2-a44e-c49a0ff25946"

# Site boundary
site_boundary <- get_site_info(deimsid = deimsid)

# Download iNaturalist occurrences
occ_eLTER <- get_site_speciesOccurrences(
  deimsid = deimsid,
  list_DS = "inat",
  show_map = FALSE,
  limit = 5000
)

# Enrich with IUCN conservation status
occ_eLTER_IUCN <- add_iucn_to_occ(
  occ_eLTER = occ_eLTER$inat
)

# Enrich with establishment means by iNaturalist, IUCN and EASIN
occ_eLTER_nativeness <- add_nativeness_to_occ(
  occ_eLTER_IUCN,
  country = site_boundary$country
)

# Enrich with EUNIS legal information
occ_eLTER_EUNIS <- add_eunis_legal_to_occ(
  occ_eLTER_nativeness
)

# Save datasets
usethis::use_data(occ_eLTER_EUNIS, overwrite = TRUE)
usethis::use_data(site_boundary, overwrite = TRUE)
