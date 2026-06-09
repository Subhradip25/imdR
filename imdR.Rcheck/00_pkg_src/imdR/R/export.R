# ── Internal: human-readable file size ───────────────────────
.file_size_str <- function(path) {
     bytes <- file.size(path)
     if (is.na(bytes)) return("unknown size")
     if (bytes >= 1024^2) {
          paste0(round(bytes / 1024^2, 2), " MB")
     } else if (bytes >= 1024) {
          paste0(round(bytes / 1024,   1), " KB")
     } else {
          paste0(bytes, " bytes")
     }
}

#' Extract a daily time series at a point location
#'
#' Extracts daily values from an IMD SpatRaster at the nearest grid
#' cell to the specified latitude/longitude and returns a data frame.
#'
#' @param imd_raster A terra SpatRaster from get_data().
#' @param lat Latitude in decimal degrees (WGS84).
#' @param lon Longitude in decimal degrees (WGS84).
#' @param file_path Character or NULL. If provided, saves output as CSV.
#' @return An invisible data frame with columns date and value.
#' @examples
#' \donttest{
#' r  <- get_data("rain", 2020, 2020, tempdir())
#' df <- to_csv(r, lat = 15.5, lon = 73.8)
#' head(df)
#'
#' # Save directly to file
#' to_csv(r, lat = 15.5, lon = 73.8,
#'        file_path = file.path(tempdir(), "panaji_rain_2020.csv"))
#' }
#' @export
to_csv <- function(imd_raster, lat, lon, file_path = NULL) {

     ext_r <- terra::ext(imd_raster)
     if (lon < ext_r$xmin || lon > ext_r$xmax ||
         lat < ext_r$ymin || lat > ext_r$ymax)
          stop("lat/lon outside IMD extent: lon ",
               ext_r$xmin, "-", ext_r$xmax,
               ", lat ", ext_r$ymin, "-", ext_r$ymax)

     cell_num <- terra::cellFromXY(imd_raster,
                                   matrix(c(lon, lat), nrow = 1))
     if (is.na(cell_num))
          stop("No valid cell for lat=", lat, ", lon=", lon)

     vals  <- as.numeric(imd_raster[cell_num])
     dates <- as.Date(names(imd_raster))
     df    <- data.frame(date = dates, value = vals)

     message(paste("Extracted", nrow(df)), "daily values",
         "| lat =", lat, "| lon =", lon, "\n")
     message(paste(paste("Non-NA days:", sum(!is.na(df$value))), "\n"))

     if (!is.null(file_path)) {
          write.csv(df, file = file_path, row.names = FALSE)
          message(paste("Saved to:", file_path, "\n"))
     }

     return(invisible(df))
}

#' Save an IMD SpatRaster as a CF-1.7 compliant NetCDF file
#'
#' Writes a multi-layer terra SpatRaster to a CF-1.7 compliant NetCDF
#' file with correct time, latitude, and longitude dimensions and
#' standard metadata attributes.
#'
#' @param imd_raster A terra SpatRaster.
#' @param file_path Character. Output .nc file path.
#' @param variable One of "rain", "tmax", "tmin".
#' @return Invisible character: the file path written.
#' @examples
#' \donttest{
#' r <- get_data("rain", 2020, 2020, tempdir())
#' to_netcdf(r, file.path(tempdir(), "rain_2020.nc"), "rain")
#'
#' # Save a boundary-extracted region
#' goa <- extract_by_boundary(r, "state", "Goa", "rain")
#' to_netcdf(goa, file.path(tempdir(), "rain_Goa_2020.nc"), "rain")
#' }
#' @export
to_netcdf <- function(imd_raster, file_path, variable = "rain") {

     labels <- list(
          rain = list(long_name = "Daily rainfall",
                      units     = "mm/day"),
          tmax = list(long_name = "Daily maximum temperature",
                      units     = "deg_C"),
          tmin = list(long_name = "Daily minimum temperature",
                      units     = "deg_C")
     )
     meta <- if (!is.null(labels[[variable]])) labels[[variable]] else
          list(long_name = variable, units = "unknown")

     dates  <- as.Date(names(imd_raster))
     n_days <- terra::nlyr(imd_raster)
     n_rows <- terra::nrow(imd_raster)
     n_cols <- terra::ncol(imd_raster)
     ext_r  <- terra::ext(imd_raster)
     res_r  <- terra::res(imd_raster)

     lons <- seq(ext_r$xmin + res_r[1] / 2,
                 ext_r$xmax - res_r[1] / 2,
                 length.out = n_cols)
     lats <- seq(ext_r$ymax - res_r[2] / 2,
                 ext_r$ymin + res_r[2] / 2,
                 length.out = n_rows)

     origin    <- as.Date("1900-01-01")
     time_vals <- as.numeric(dates - origin)

     dim_lon  <- ncdf4::ncdim_def("lon", "degrees_east",  lons)
     dim_lat  <- ncdf4::ncdim_def("lat", "degrees_north", lats)
     dim_time <- ncdf4::ncdim_def(
          "time",
          paste("days since", format(origin, "%Y-%m-%d")),
          time_vals, unlim = TRUE
     )

     nc_var <- ncdf4::ncvar_def(
          name     = variable,
          units    = meta$units,
          dim      = list(dim_lon, dim_lat, dim_time),
          missval  = -999.0,
          longname = meta$long_name,
          prec     = "float"
     )

     message(paste("Writing NetCDF:", file_path, "\n"))
     message(paste("Dimensions: lon =", n_cols, "| lat =", n_rows,
         "| time =", n_days, "\n"))

     nc <- ncdf4::nc_create(file_path, vars = list(nc_var))

     ncdf4::ncatt_put(nc, 0,      "Conventions",   "CF-1.7")
     ncdf4::ncatt_put(nc, 0,      "title",         "IMD gridded data")
     ncdf4::ncatt_put(nc, 0,      "source",        "https://imdpune.gov.in/")
     ncdf4::ncatt_put(nc, 0,      "created_by",    "imdR")
     ncdf4::ncatt_put(nc, 0,      "history",
                      paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "R imdR"))
     ncdf4::ncatt_put(nc, "lon",  "standard_name", "longitude")
     ncdf4::ncatt_put(nc, "lon",  "long_name",     "longitude")
     ncdf4::ncatt_put(nc, "lon",  "axis",          "X")
     ncdf4::ncatt_put(nc, "lat",  "standard_name", "latitude")
     ncdf4::ncatt_put(nc, "lat",  "long_name",     "latitude")
     ncdf4::ncatt_put(nc, "lat",  "axis",          "Y")
     ncdf4::ncatt_put(nc, "time", "standard_name", "time")
     ncdf4::ncatt_put(nc, "time", "calendar",      "standard")
     ncdf4::ncatt_put(nc, "time", "axis",          "T")

     pb <- utils::txtProgressBar(min = 0, max = n_days, style = 3)
     for (i in seq_len(n_days)) {
          layer_mat <- as.matrix(imd_raster[[i]], wide = TRUE)
          layer_mat[is.na(layer_mat)] <- -999.0
          ncdf4::ncvar_put(nc, variable,
                           vals  = t(layer_mat),
                           start = c(1, 1, i),
                           count = c(n_cols, n_rows, 1))
          utils::setTxtProgressBar(pb, i)
     }
     close(pb)
     ncdf4::nc_close(nc)

     message(paste("\nSaved:", file_path,
         "(", .file_size_str(file_path)), ")\n")
     return(invisible(file_path))
}

#' Save an IMD SpatRaster as a compressed GeoTIFF
#'
#' Writes a multi-layer terra SpatRaster to a DEFLATE-compressed,
#' tiled GeoTIFF suitable for use in QGIS, ArcGIS, Python (rasterio),
#' and other spatial software.
#'
#' @param imd_raster A terra SpatRaster.
#' @param file_path Character. Output .tif file path.
#' @return Invisible character: the file path written.
#' @examples
#' \donttest{
#' r <- get_data("rain", 2020, 2020, tempdir())
#' to_geotiff(r, file.path(tempdir(), "rain_2020.tif"))
#'
#' # Save a boundary-extracted region
#' goa <- extract_by_boundary(r, "state", "Goa", "rain")
#' to_geotiff(goa, file.path(tempdir(), "rain_Goa_2020.tif"))
#' }
#' @export
to_geotiff <- function(imd_raster, file_path) {

     message(paste("Writing GeoTIFF:", file_path, "\n"))
     message(paste("Layers:", terra::nlyr(imd_raster)),
         "| Extent:", as.character(terra::ext(imd_raster)), "\n")

     terra::writeRaster(imd_raster,
                        filename  = file_path,
                        filetype  = "GTiff",
                        overwrite = TRUE,
                        gdal      = c("COMPRESS=DEFLATE",
                                      "PREDICTOR=2",
                                      "TILED=YES"))

     message(paste("Saved:", file_path,
         "(", .file_size_str(file_path)), ")\n")
     return(invisible(file_path))
}
