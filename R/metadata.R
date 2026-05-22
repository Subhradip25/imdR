# Internal package constants — imd_meta, imd_endpoints, imd_palette
# are stored in R/sysdata.rda (created via usethis::use_data(internal=TRUE))
# They are accessible to all package functions but not exported to users.

#' India state boundaries (SOI-approved)
#'
#' An sf object with boundaries for all 36 Indian states and union territories,
#' sourced from Survey of India (SOI) shapefiles, reprojected to WGS84.
#'
#' @format An sf data frame with 36 rows and columns state_name and geometry.
"india_states"

#' India district boundaries (SOI-approved)
#'
#' An sf object with boundaries for 808 Indian districts,
#' sourced from Survey of India (SOI) shapefiles, reprojected to WGS84.
#'
#' @format An sf data frame with 808 rows and columns state_name,
#'   district_name, and geometry.
"india_districts"
