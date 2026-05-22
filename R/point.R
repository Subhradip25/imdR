#' Extract daily time series for a single variable at a point
#'
#' @param lat Latitude in decimal degrees.
#' @param lon Longitude in decimal degrees.
#' @param variable One of "rain", "tmax", "tmin".
#' @param start_yr Start year.
#' @param end_yr End year.
#' @param file_dir Directory for .grd files.
#' @param save_csv Save output as CSV? Default TRUE.
#' @return Invisible data frame with columns date, lat, lon, variable.
#' @export
get_point <- function(lat, lon, variable, start_yr, end_yr,
                      file_dir = ".", save_csv = TRUE) {

     imd_raster <- get_data(variable, start_yr, end_yr, file_dir)

     if (is.list(imd_raster) && !inherits(imd_raster, "SpatRaster"))
          imd_raster <- do.call(c, imd_raster)

     df              <- to_csv(imd_raster, lat, lon, file_path = NULL)
     colnames(df)[2] <- variable
     df$lat          <- lat
     df$lon          <- lon
     df              <- df[, c("date", "lat", "lon", variable)]

     if (save_csv) {
          fname <- file.path(
               path.expand(file_dir),
               paste0(variable, "_", lat, "N_", lon, "E_",
                      start_yr, "_", end_yr, ".csv"))
          write.csv(df, fname, row.names = FALSE)
          cat("Saved:", fname, "\n")
     }

     return(invisible(df))
}

#' Extract daily time series for all variables at a point
#'
#' @param lat Latitude in decimal degrees.
#' @param lon Longitude in decimal degrees.
#' @param start_yr Start year.
#' @param end_yr End year.
#' @param file_dir Directory for .grd files.
#' @param save_csv Save merged output as CSV? Default TRUE.
#' @return Invisible data frame with columns date, lat, lon,
#'   rain, tmax, tmin, dtr.
#' @export
get_point_all <- function(lat, lon, start_yr, end_yr,
                          file_dir = ".", save_csv = TRUE) {

     cat("=== Extracting all variables at lat =",
         lat, ", lon =", lon, "===\n\n")

     df_rain <- get_point(lat, lon, "rain", start_yr, end_yr,
                          file_dir, save_csv = FALSE)
     df_tmax <- get_point(lat, lon, "tmax", start_yr, end_yr,
                          file_dir, save_csv = FALSE)
     df_tmin <- get_point(lat, lon, "tmin", start_yr, end_yr,
                          file_dir, save_csv = FALSE)

     df <- merge(df_rain,
                 df_tmax[, c("date", "tmax")], by = "date", all = TRUE)
     df <- merge(df,
                 df_tmin[, c("date", "tmin")], by = "date", all = TRUE)
     df <- df[order(df$date),
              c("date", "lat", "lon", "rain", "tmax", "tmin")]
     df$dtr <- round(df$tmax - df$tmin, 3)

     if (save_csv) {
          fname <- file.path(
               path.expand(file_dir),
               paste0("imd_all_", lat, "N_", lon, "E_",
                      start_yr, "_", end_yr, ".csv"))
          write.csv(df, fname, row.names = FALSE)
          cat("Saved:", fname, "\n")
     }

     return(invisible(df))
}
