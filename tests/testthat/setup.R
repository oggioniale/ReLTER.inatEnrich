# tests/testthat/setup.R
options(device = function(...) pdf(tempfile(fileext = ".pdf"), ...))

df            <- ReLTER.inatEnrich::occ_eLTER_legal
site_boundary <- ReLTER.inatEnrich::site_boundary