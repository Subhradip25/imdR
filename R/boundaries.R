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
#' list_districts()
#' list_districts("Goa")
#' @export
list_districts <- function(state = NULL) {
     if (is.null(state))
          return(sort(india_districts$district_name))
     matched <- india_districts[
          grepl(state, india_districts$state_name,
                ignore.case = TRUE), ]
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
#' goa       <- get_boundary("state", "Goa")
#' north_goa <- get_boundary("district", "North Goa")
#' @export
get_boundary <- function(level = "state", name) {

     if (!level %in% c("state", "district"))
          stop("level must be 'state' or 'district'")

     if (level == "state") {
          out <- india_states[
               grepl(name, india_states$state_name,
                     ignore.case = TRUE), ]
     } else {
          out <- india_districts[
               grepl(name, india_districts$district_name,
                     ignore.case = TRUE), ]
     }

     if (nrow(out) == 0)
          stop("No ", level, " found matching: '", name, "'")

     message(paste("Found:", nrow(out), level,
                   "feature(s) for '", name, "'"))
     return(out)
}

# ── Internal: convert sf boundary to SpatVector safely ────────
.boundary_to_vect <- function(boundary) {
     terra::vect(sf::st_as_sf(boundary))
}

# ── Internal: SpatRaster to long-format data frame ────────────
.raster_to_long <- function(r_masked, variable) {

     dates  <- as.Date(names(r_masked))
     n_cell <- terra::ncell(r_masked)

     coords        <- as.data.frame(
          terra::xyFromCell(r_masked, 1:n_cell)
     )
     names(coords) <- c("lon", "lat")

     vals  <- as.matrix(r_masked, wide = FALSE)
     valid <- which(rowSums(!is.na(vals)) > 0)

     message(paste("Valid land cells :", length(valid)))
     message(paste("Total rows in CSV:", length(valid) * length(dates)))
     message("Building long-format table...")

     df_list <- lapply(valid, function(i) {
          data.frame(
               date  = dates,
               lat   = round(coords$lat[i], 4),
               lon   = round(coords$lon[i], 4),
               value = round(vals[i, ], 4),
               stringsAsFactors = FALSE
          )
     })

     df           <- do.call(rbind, df_list)
     names(df)[4] <- variable
     df           <- df[order(df$date, df$lat, df$lon), ]
     rownames(df) <- NULL
     df
}

#' Extract IMD raster masked to a state or district boundary
#'
#' Crops and masks an IMD SpatRaster to any named state or district
#' using bundled SOI-approved boundaries. Supports three output formats:
#' \itemize{
#'   \item \code{"netcdf"} -- CF-1.7 compliant NetCDF
#'   \item \code{"geotiff"} -- Multi-band GeoTIFF, opens in QGIS/ArcGIS
#'   \item \code{"csv"} -- Long-format table: date, lat, lon, value
#' }
#'
#' @param imd_raster A SpatRaster or named list from get_data().
#' @param level "state" (default) or "district".
#' @param name State or district name (partial match allowed).
#' @param variable Variable name for output column and filename.
#' @param save Save output to disk? Default FALSE.
#' @param format "netcdf" (default), "geotiff", or "csv".
#' @param file_dir Output directory.
#' @return Invisible masked SpatRaster.
#' @examples
#' \donttest{
#' r <- get_data("rain", 2020, 2020, tempdir())
#'
#' # Return masked raster without saving
#' nagaland_rain <- extract_by_boundary(r, "state", "Nagaland", "rain")
#'
#' # State — NetCDF
#' extract_by_boundary(r, "state", "Nagaland", "rain",
#'   save = TRUE, format = "netcdf", file_dir = tempdir())
#'
#' # State — GeoTIFF (QGIS/ArcGIS)
#' extract_by_boundary(r, "state", "Nagaland", "rain",
#'   save = TRUE, format = "geotiff", file_dir = tempdir())
#'
#' # State — CSV (all grid points x all days)
#' extract_by_boundary(r, "state", "Nagaland", "rain",
#'   save = TRUE, format = "csv", file_dir = tempdir())
#'
#' # District — all formats work the same way
#' extract_by_boundary(r, "district", "North Goa", "rain",
#'   save = TRUE, format = "csv", file_dir = tempdir())
#' }
#' @export
extract_by_boundary <- function(imd_raster,
                                level    = "state",
                                name     = NULL,
                                variable = "rain",
                                save     = FALSE,
                                format   = "netcdf",
                                file_dir) {

     if (is.null(name))
          stop("Please provide a 'name' for the state or district.")

     if (!format %in% c("netcdf", "geotiff", "csv"))
          stop("format must be 'netcdf', 'geotiff', or 'csv'.")

     if (is.list(imd_raster) && !inherits(imd_raster, "SpatRaster")) {
          message("Stacking multi-year list...")
          imd_raster <- do.call(c, imd_raster)
     }

     boundary <- get_boundary(level, name)
     bv       <- .boundary_to_vect(boundary)
     r_crop   <- terra::crop(imd_raster, bv)
     r_masked <- terra::mask(r_crop, bv)

     message(paste("Region  :", name, "(", level, ")"))
     message(paste("Grid    :", terra::nrow(r_masked), "rows x",
                   terra::ncol(r_masked), "cols"))
     message(paste("Layers  :", terra::nlyr(r_masked), "days"))
     message(paste("Format  :", format))

     if (save) {

          clean_name <- gsub("[^A-Za-z0-9]", "_", name)
          dates      <- as.Date(names(r_masked))
          yr1        <- format(min(dates), "%Y")
          yr2        <- format(max(dates), "%Y")
          yr_tag     <- if (yr1 == yr2) yr1 else paste0(yr1, "_", yr2)

          if (format == "netcdf") {

               fname <- file.path(
                    path.expand(file_dir),
                    paste0(variable, "_", clean_name, "_", yr_tag, ".nc"))
               to_netcdf(r_masked, fname, variable)

          } else if (format == "geotiff") {

               fname <- file.path(
                    path.expand(file_dir),
                    paste0(variable, "_", clean_name, "_", yr_tag, ".tif"))
               to_geotiff(r_masked, fname)

          } else if (format == "csv") {

               df    <- .raster_to_long(r_masked, variable)
               fname <- file.path(
                    path.expand(file_dir),
                    paste0(variable, "_", clean_name,
                           "_all_grids_", yr_tag, ".csv"))
               write.csv(df, fname, row.names = FALSE)
               message(paste("Saved:", fname))
               message(paste("Rows:", nrow(df),
                             "| Columns: date, lat, lon,", variable))
          }
     }

     return(invisible(r_masked))
}
