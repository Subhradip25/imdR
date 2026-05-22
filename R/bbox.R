#' Extract IMD data within a bounding box
#'
#' @param lat_min Minimum latitude.
#' @param lat_max Maximum latitude.
#' @param lon_min Minimum longitude.
#' @param lon_max Maximum longitude.
#' @param variable One of "rain", "tmax", "tmin".
#' @param start_yr Start year.
#' @param end_yr End year.
#' @param file_dir Directory for files.
#' @param format "netcdf" (default) or "geotiff".
#' @param save Save output? Default TRUE.
#' @return Invisible SpatRaster of the cropped region.
#' @export
get_bbox <- function(lat_min, lat_max, lon_min, lon_max,
                     variable, start_yr, end_yr,
                     file_dir = ".",
                     format   = "netcdf",
                     save     = TRUE) {

     if (!variable %in% c("rain", "tmax", "tmin"))
          stop("variable must be 'rain', 'tmax', or 'tmin'")
     if (!format %in% c("netcdf", "geotiff"))
          stop("format must be 'netcdf' or 'geotiff'")
     if (lat_min >= lat_max) stop("lat_min must be < lat_max")
     if (lon_min >= lon_max) stop("lon_min must be < lon_max")

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

     imd_raster <- get_data(variable, start_yr, end_yr, file_dir)

     if (is.list(imd_raster) && !inherits(imd_raster, "SpatRaster")) {
          cat("Stacking multi-year list for bbox extraction...\n")
          imd_raster <- do.call(c, imd_raster)
     }

     r_crop <- terra::crop(imd_raster,
                           terra::ext(lon_min, lon_max, lat_min, lat_max))

     cat("Cropped:", terra::nrow(r_crop), "x", terra::ncol(r_crop),
         "x", terra::nlyr(r_crop), "layers\n")

     if (save) {
          bbox_tag <- paste0(lat_min, "N_", lat_max, "N_",
                             lon_min, "E_", lon_max, "E")
          yr_tag   <- paste0(start_yr, "_", end_yr)
          if (format == "netcdf") {
               fname <- file.path(path.expand(file_dir),
                                  paste0(variable, "_", bbox_tag,
                                         "_", yr_tag, ".nc"))
               to_netcdf(r_crop, fname, variable)
          } else {
               fname <- file.path(path.expand(file_dir),
                                  paste0(variable, "_", bbox_tag,
                                         "_", yr_tag, ".tif"))
               to_geotiff(r_crop, fname)
          }
     }

     return(invisible(r_crop))
}
