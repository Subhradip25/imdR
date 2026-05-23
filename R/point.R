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
#' @examples
#' \dontrun{
#' # Extract daily rainfall at Panaji, Goa
#' df <- get_point(lat = 15.5, lon = 73.8,
#'                 variable = "rain",
#'                 start_yr = 2020, end_yr = 2020,
#'                 file_dir = "~/imdR_data")
#' head(df)
#'
#' # Extract temperature
#' df_tmax <- get_point(lat = 15.5, lon = 73.8,
#'                      variable = "tmax",
#'                      start_yr = 2020, end_yr = 2020,
#'                      file_dir = "~/imdR_data")
#' }
#' @export
get_point <- function(lat, lon, variable, start_yr, end_yr,
                      file_dir = ".", save_csv = TRUE) {

     imd_raster <- get_data(variable, start_yr, end_yr, file_dir)

     # Extract year by year to avoid stacking large rasters into memory.
     # Stacking 10+ years at once causes memory errors on Windows.
     if (is.list(imd_raster) && !inherits(imd_raster, "SpatRaster")) {
          cat("Extracting point data year by year...\n")
          df_list <- lapply(names(imd_raster), function(yr) {
               to_csv(imd_raster[[yr]], lat, lon)
          })
          df <- do.call(rbind, df_list)
     } else {
          df <- to_csv(imd_raster, lat, lon)
     }

     colnames(df)[2] <- variable
     df$lat          <- lat
     df$lon          <- lon
     df              <- df[, c("date", "lat", "lon", variable)]
     df              <- df[order(df$date), ]
     rownames(df)    <- NULL

     cat("Total rows:", nrow(df), "| Date range:",
         as.character(min(df$date)), "to",
         as.character(max(df$date)), "\n")

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
#' Downloads or reads rain, tmax, and tmin at a location and merges
#' them into a single data frame that also includes diurnal temperature
#' range (DTR). Extraction is done year by year to avoid memory issues
#' with long time series on Windows.
#'
#' @param lat Latitude in decimal degrees.
#' @param lon Longitude in decimal degrees.
#' @param start_yr Start year.
#' @param end_yr End year.
#' @param file_dir Directory for .grd files.
#' @param save_csv Save merged output as CSV? Default TRUE.
#' @return Invisible data frame with columns date, lat, lon,
#'   rain, tmax, tmin, dtr.
#' @examples
#' \dontrun{
#' # Extract rain, tmax, tmin and DTR at Panaji, Goa
#' df <- get_point_all(lat = 15.5, lon = 73.8,
#'                     start_yr = 2020, end_yr = 2020,
#'                     file_dir = "~/imdR_data")
#' head(df)
#'
#' # Long time series — works on Windows without memory errors
#' df <- get_point_all(lat = 15.5, lon = 73.8,
#'                     start_yr = 1985, end_yr = 2020,
#'                     file_dir = "~/imdR_data")
#' nrow(df)
#' }
#' @export
get_point_all <- function(lat, lon, start_yr, end_yr,
                          file_dir = ".", save_csv = TRUE) {

     cat("=== Extracting all variables at lat =",
         lat, ", lon =", lon, "===\n\n")

     cat("--- Rainfall ---\n")
     df_rain <- get_point(lat, lon, "rain", start_yr, end_yr,
                          file_dir, save_csv = FALSE)

     cat("\n--- Max Temperature ---\n")
     df_tmax <- get_point(lat, lon, "tmax", start_yr, end_yr,
                          file_dir, save_csv = FALSE)

     cat("\n--- Min Temperature ---\n")
     df_tmin <- get_point(lat, lon, "tmin", start_yr, end_yr,
                          file_dir, save_csv = FALSE)

     # Merge all three on date
     df <- merge(df_rain,
                 df_tmax[, c("date", "tmax")],
                 by = "date", all = TRUE)
     df <- merge(df,
                 df_tmin[, c("date", "tmin")],
                 by = "date", all = TRUE)
     df <- df[order(df$date),
              c("date", "lat", "lon", "rain", "tmax", "tmin")]
     df$dtr    <- round(df$tmax - df$tmin, 3)
     rownames(df) <- NULL

     cat("\n--- Merged summary ---\n")
     cat("Rows:", nrow(df), "\n")
     cat("Date range:", as.character(min(df$date, na.rm = TRUE)),
         "to", as.character(max(df$date, na.rm = TRUE)), "\n")
     cat("Columns:", paste(names(df), collapse = ", "), "\n")
     cat("Non-NA rain days:", sum(!is.na(df$rain)), "\n")
     cat("Non-NA tmax days:", sum(!is.na(df$tmax)), "\n")
     cat("Non-NA tmin days:", sum(!is.na(df$tmin)), "\n")

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
