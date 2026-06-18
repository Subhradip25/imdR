#' Extract IMD data within a bounding box
#'
#' Crops IMD raster data to a user-defined latitude/longitude bounding
#' box. Useful for custom regions such as the Indo-Gangetic Plains,
#' Western Ghats, or any area not matching a state or district boundary.
#' Supports three output formats: NetCDF, GeoTIFF, and long-format CSV.
#'
#' @param lat_min Numeric. Minimum latitude.
#' @param lat_max Numeric. Maximum latitude.
#' @param lon_min Numeric. Minimum longitude.
#' @param lon_max Numeric. Maximum longitude.
#' @param variable One of "rain", "tmax", "tmin".
#' @param start_yr Integer. Start year.
#' @param end_yr Integer. End year.
#' @param file_dir Character. Directory for files.
#' @param format "netcdf" (default), "geotiff", or "csv".
#' @param save Logical. Save output? Default TRUE.
#' @return Invisible SpatRaster of the cropped region.
#' @examples
#' \donttest{
#' # Indo-Gangetic Plains -- NetCDF
#' get_bbox(lat_min = 24, lat_max = 30,
#'          lon_min = 73, lon_max = 88,
#'          variable = "rain",
#'          start_yr = 2020, end_yr = 2020,
#'          file_dir = tempdir(),
#'          format   = "netcdf")
#'
#' # Western Ghats -- GeoTIFF
#' get_bbox(lat_min = 8,  lat_max = 21,
#'          lon_min = 73, lon_max = 78,
#'          variable = "rain",
#'          start_yr = 2020, end_yr = 2020,
#'          file_dir = tempdir(),
#'          format   = "geotiff")
#'
#' # Northeast India -- CSV (all grid points x all days)
#' get_bbox(lat_min = 22, lat_max = 29,
#'          lon_min = 89, lon_max = 97,
#'          variable = "rain",
#'          start_yr = 2020, end_yr = 2020,
#'          file_dir = tempdir(),
#'          format   = "csv")
#' }
#' @export
get_bbox <- function(lat_min, lat_max, lon_min, lon_max,
                     variable, start_yr, end_yr,
                     file_dir,
                     format   = "netcdf",
                     save     = TRUE) {

     if (!variable %in% c("rain", "tmax", "tmin"))
          stop("variable must be 'rain', 'tmax', or 'tmin'")
     if (!format %in% c("netcdf", "geotiff", "csv"))
          stop("format must be 'netcdf', 'geotiff', or 'csv'.")
     if (lat_min >= lat_max) stop("lat_min must be < lat_max")
     if (lon_min >= lon_max) stop("lon_min must be < lon_max")

     # Clip to IMD extent if needed
     imd_ext <- list(rain = c(66.5, 100.0, 6.5, 38.5),
                     tmax = c(67.5,  97.5, 7.5, 37.5),
                     tmin = c(67.5,  97.5, 7.5, 37.5))
     ex <- imd_ext[[variable]]

     if (lon_min < ex[1] || lon_max > ex[2] ||
         lat_min < ex[3] || lat_max > ex[4]) {
          warning("Bounding box partially outside IMD extent. Clipping.")
          lon_min <- max(lon_min, ex[1]); lon_max <- min(lon_max, ex[2])
          lat_min <- max(lat_min, ex[3]); lat_max <- min(lat_max, ex[4])
     }

     message("=== get_bbox() ===")
     message(paste("Variable:", variable,
                   "| Years:", start_yr, "-", end_yr))
     message(paste("Bounding box: lat", lat_min, "-", lat_max,
                   "| lon", lon_min, "-", lon_max))
     message(paste("Format  :", format))

     imd_raster <- get_data(variable, start_yr, end_yr, file_dir)

     if (is.list(imd_raster) && !inherits(imd_raster, "SpatRaster")) {
          message("Stacking multi-year list for bbox extraction...")
          imd_raster <- do.call(c, imd_raster)
     }

     r_crop <- terra::crop(
          imd_raster,
          terra::ext(lon_min, lon_max, lat_min, lat_max)
     )

     message(paste("Cropped:", terra::nrow(r_crop), "rows x",
                   terra::ncol(r_crop), "cols x",
                   terra::nlyr(r_crop), "layers"))

     if (save) {

          bbox_tag <- paste0(lat_min, "N_", lat_max, "N_",
                             lon_min, "E_", lon_max, "E")
          yr_tag   <- if (start_yr == end_yr)
               as.character(start_yr) else
                    paste0(start_yr, "_", end_yr)

          if (format == "netcdf") {

               fname <- file.path(
                    path.expand(file_dir),
                    paste0(variable, "_bbox_", bbox_tag, "_", yr_tag, ".nc"))
               to_netcdf(r_crop, fname, variable)

          } else if (format == "geotiff") {

               fname <- file.path(
                    path.expand(file_dir),
                    paste0(variable, "_bbox_", bbox_tag, "_", yr_tag, ".tif"))
               to_geotiff(r_crop, fname)

          } else if (format == "csv") {

               df    <- .raster_to_long(r_crop, variable)
               fname <- file.path(
                    path.expand(file_dir),
                    paste0(variable, "_bbox_", bbox_tag,
                           "_all_grids_", yr_tag, ".csv"))
               write.csv(df, fname, row.names = FALSE)
               message(paste("Saved:", fname))
               message(paste("Rows:", nrow(df),
                             "| Columns: date, lat, lon,", variable))
          }
     }

     return(invisible(r_crop))
}
