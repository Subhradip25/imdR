#' List all state names in the bundled SOI shapefile
#'
#' @return A sorted character vector of 36 state/UT names.
#' @examples
#' list_states()
#' @export
list_states <- function() {
     sort(india_states$state_name)
}

#' List district names, optionally filtered by state
#'
#' @param state Character or NULL. Partial match, case-insensitive.
#' @return A sorted character vector of district names.
#' @examples
#' # All districts
#' list_districts()
#'
#' # Districts in Goa
#' list_districts("Goa")
#'
#' # Districts in Kerala
#' list_districts("Kerala")
#' @export
list_districts <- function(state = NULL) {
     if (is.null(state)) return(sort(india_districts$district_name))
     matched <- india_districts[
          grepl(state, india_districts$state_name, ignore.case = TRUE), ]
     if (nrow(matched) == 0)
          stop("No state found matching: '", state, "'")
     sort(matched$district_name)
}

#' Get the sf boundary for a named state or district
#'
#' @param level "state" (default) or "district".
#' @param name State or district name (partial match allowed).
#' @return An sf object with the matching boundary.
#' @examples
#' # Get state boundary
#' goa <- get_boundary("state", "Goa")
#'
#' # Get district boundary
#' north_goa <- get_boundary("district", "North Goa")
#' @export
get_boundary <- function(level = "state", name) {

     if (!level %in% c("state", "district"))
          stop("level must be 'state' or 'district'")

     if (level == "state") {
          out <- india_states[
               grepl(name, india_states$state_name, ignore.case = TRUE), ]
     } else {
          out <- india_districts[
               grepl(name, india_districts$district_name,
                     ignore.case = TRUE), ]
     }

     if (nrow(out) == 0)
          stop("No ", level, " found matching: '", name, "'")

     cat("Found:", nrow(out), level, "feature(s) for '", name, "'\n")
     return(out)
}

#' Extract IMD raster masked to a state or district boundary
#'
#' @param imd_raster A SpatRaster or named list from get_data().
#' @param level "state" (default) or "district".
#' @param name State or district name.
#' @param variable Variable name for output filename.
#' @param save Save output? Default FALSE.
#' @param format "netcdf" or "geotiff".
#' @param file_dir Output directory.
#' @return Invisible masked SpatRaster.
#' @examples
#' \dontrun{
#' r <- get_data("rain", 2020, 2020, "~/imdR_data")
#'
#' # Extract for a state
#' goa_rain <- extract_by_boundary(r,
#'   level    = "state",
#'   name     = "Goa",
#'   variable = "rain")
#'
#' # Extract for a district and save as NetCDF
#' kerala_rain <- extract_by_boundary(r,
#'   level    = "state",
#'   name     = "Kerala",
#'   variable = "rain",
#'   save     = TRUE,
#'   format   = "netcdf",
#'   file_dir = "~/imdR_data")
#' }
#' @export
extract_by_boundary <- function(imd_raster,
                                level    = "state",
                                name     = NULL,
                                variable = "rain",
                                save     = FALSE,
                                format   = "netcdf",
                                file_dir = ".") {

     if (is.null(name))
          stop("Please provide a 'name' for the state or district.")

     if (is.list(imd_raster) && !inherits(imd_raster, "SpatRaster")) {
          cat("Stacking multi-year list for boundary extraction...\n")
          imd_raster <- do.call(c, imd_raster)
     }

     boundary <- get_boundary(level, name)
     r_crop   <- terra::crop(imd_raster, terra::vect(boundary))
     r_masked <- terra::mask(r_crop,     terra::vect(boundary))

     cat("Region:", name, "(", level, ") |",
         terra::nrow(r_masked), "x", terra::ncol(r_masked),
         "x", terra::nlyr(r_masked), "layers\n")

     if (save) {
          clean_name <- gsub("[^A-Za-z0-9]", "_", name)
          yr_tag     <- paste0(names(r_masked)[1], "_",
                               names(r_masked)[terra::nlyr(r_masked)])
          if (format == "netcdf") {
               fname <- file.path(path.expand(file_dir),
                                  paste0(variable, "_", clean_name,
                                         "_", yr_tag, ".nc"))
               to_netcdf(r_masked, fname, variable)
          } else {
               fname <- file.path(path.expand(file_dir),
                                  paste0(variable, "_", clean_name,
                                         "_", yr_tag, ".tif"))
               to_geotiff(r_masked, fname)
          }
     }

     return(invisible(r_masked))
}
